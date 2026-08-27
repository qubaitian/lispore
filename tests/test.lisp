(in-package #:lispore.tests)

(defvar *tests* nil)

(defmacro deftest (name () &body body)
  `(progn
     (defun ,name () ,@body)
     (pushnew ',name *tests*)))

(defun check (condition format-control &rest format-arguments)
  (unless condition
    (error (apply #'format nil format-control format-arguments))))

(deftest terminal-writes-text ()
  (let ((terminal (make-terminal-emulator :width 8 :height 2)))
    (feed-terminal terminal "hello")
    (check (string= "hello   " (first (screen-lines terminal)))
           "The first screen line does not contain the text.")))

(deftest utf8-round-trips-unicode ()
  (let ((bytes (encode-utf8 "hé界")))
    (check (equalp #(104 195 169 231 149 140) bytes)
           "UTF-8 encoding has unexpected bytes.")
    (multiple-value-bind (text pending)
        (decode-utf8-chunk bytes)
      (check (string= "hé界" text)
             "UTF-8 decoding has unexpected text.")
      (check (null pending)
             "UTF-8 decoding leaves unexpected pending bytes."))))

(deftest utf8-decoder-keeps-split-character ()
  (let ((bytes (encode-utf8 "界")))
    (multiple-value-bind (first pending)
        (decode-utf8-chunk (subseq bytes 0 2))
      (check (string= "" first)
             "UTF-8 decoding emits an incomplete character.")
      (multiple-value-bind (second remaining)
          (decode-utf8-chunk (subseq bytes 2) pending)
        (check (string= "界" second)
               "UTF-8 decoding loses a split character.")
        (check (null remaining)
               "UTF-8 decoding keeps complete character bytes.")))))

(deftest pty-session-runs-a-shell ()
  (let ((session (start-shell :shell "/bin/sh" :width 80 :height 24)))
    (unwind-protect
        (progn
          (check (session-open-p session)
                 "The PTY session does not start open.")
          (check (integerp (lispore:pty-master session))
                 "The PTY master accessor does not return a descriptor.")
          (resize-session session 100 40)
          (write-input session
                       (format nil "stty size; printf 'lispore-marker\\n'; exit~%"))
          (let ((output (with-output-to-string (stream)
                          (loop for chunk = (read-output session)
                                while chunk
                                do (write-string chunk stream)))))
            (check (search "40 100" output)
                   "The shell does not observe the resized PTY.")
            (check (search "lispore-marker" output)
                   "The shell output does not contain the marker.")))
      (close-session session))
    (check (not (session-open-p session))
           "The PTY session remains open after close.")))

(deftest terminal-applies-ansi-cursor-and-erase ()
  (let ((terminal (make-terminal-emulator :width 8 :height 2)))
    (feed-terminal terminal (format nil "abc~C[2J~C[Hxy" #\Escape #\Escape))
    (check (string= "xy      " (first (screen-lines terminal)))
           "ANSI cursor or erase handling is incorrect.")
    (multiple-value-bind (row column)
        (cursor-position terminal)
      (check (and (= row 1) (= column 3))
             "The cursor position is incorrect."))))

(deftest terminal-keeps-csi-device-query-quiet ()
  (let ((terminal (make-terminal-emulator :width 8 :height 2)))
    (feed-terminal terminal (format nil "abc~C[cx" #\Escape))
    (check (string= "abcx    " (first (screen-lines terminal)))
           "CSI device attributes reset the terminal unexpectedly.")))

(deftest terminal-exits-osc-at-ascii-bell ()
  (let* ((terminal (make-terminal-emulator :width 20 :height 3))
         (escape (string #\Escape))
         ;; ASCII BEL ends an OSC sequence.
         (text (concatenate 'string
                            escape "]0;title"
                            (string (code-char 7))
                            "prompt")))
    (feed-terminal terminal text)
    (check (search "prompt" (first (screen-lines terminal)))
           "OSC does not end at ASCII BEL.")))

(deftest terminal-erases-from-cursor-to-line-end ()
  (let ((terminal (make-terminal-emulator :width 8 :height 2)))
    (feed-terminal terminal (format nil "abcdef~C[3G~C[K" #\Escape #\Escape))
    (check (string= "ab      " (first (screen-lines terminal)))
           "CSI erase-line does not erase through the line end.")
    (let ((second-terminal (make-terminal-emulator :width 8 :height 2)))
      (feed-terminal second-terminal
                     (format nil "abcdef~C[3G~C[1K" #\Escape #\Escape))
      (check (string= "   def  " (first (screen-lines second-terminal)))
             "CSI erase-line does not erase through the cursor."))))

(deftest terminal-renders-independent-lines ()
  (let ((terminal (make-terminal-emulator :width 3 :height 2)))
    (feed-terminal terminal (format nil "a~C~Cb" #\Return #\Newline))
    (check (search (format nil "a  ~C~Cb" #\Return #\Newline)
                   (render-terminal terminal))
           "Rendered rows do not start at the first column.")))

(deftest terminal-keeps-sgr-style-on-screen-cells ()
  (let ((terminal (make-terminal-emulator :width 8 :height 2)))
    (feed-terminal terminal (format nil "~C[31mred~C[0mplain"
                                    #\Escape #\Escape))
    (let ((styled (cell-at terminal 1 1))
          (plain (cell-at terminal 1 4)))
      (check (and (char= #\r (screen-cell-character styled))
                  (member 31 (screen-cell-style styled)))
             "The terminal does not store SGR style.")
      (check (null (screen-cell-style plain))
             "The terminal does not clear SGR style."))))

(deftest passthrough-frontend-drains-a-session ()
  (let ((session (start-shell :shell "/bin/sh" :width 80 :height 24)))
    (unwind-protect
        (progn
          (write-input session (format nil "printf 'passthrough-marker\\n'; exit~%"))
          (check (integerp (run-passthrough :session session
                                             :input-fd nil
                                             :output-fd nil))
                 "The passthrough frontend has no exit status."))
      (close-session session))))

(deftest emulated-frontend-updates-a-screen ()
  (let ((session (start-shell :shell "/bin/sh" :width 80 :height 24))
        (terminal (make-terminal-emulator :width 80 :height 24)))
    (unwind-protect
        (progn
          (write-input session (format nil "printf 'emulated-marker\\n'; exit~%"))
          (multiple-value-bind (status result)
              (run-emulated :session session
                            :terminal terminal
                            :input-fd nil
                            :output-fd nil)
            (check (integerp status)
                   "The emulated frontend has no exit status.")
            (check (some (lambda (line) (search "emulated-marker" line))
                         (screen-lines result))
                   "The emulated frontend loses shell output.")))
      (close-session session))))

(defun terminal-settings (fd)
  "Return terminal settings for FD as text."
  (uiop:run-program (list "stty" "-g")
                    :input (format nil "/dev/fd/~D" fd)
                    :output :string))

(deftest raw-terminal-restores-after-normal-exit-and-error ()
  (let ((session (start-shell :shell "/bin/sh" :width 80 :height 24)))
    (unwind-protect
        (progn
          (let ((fd (lispore:pty-master session)))
            (check (tty-p fd)
                 "The PTY master is not a terminal descriptor.")
            (let ((before (terminal-settings fd)))
              (call-with-raw-terminal (lambda () (values)) :fd fd)
              (check (string= before (terminal-settings fd))
                     "Normal raw-terminal exit does not restore settings.")
              (let ((raised nil))
                (handler-case
                    (call-with-raw-terminal
                     (lambda () (error "expected raw-terminal error"))
                     :fd fd)
                  (error () (setf raised t)))
                (check raised
                       "The raw-terminal body does not propagate errors.")
                (check (string= before (terminal-settings fd))
                       "Error raw-terminal exit does not restore settings.")))))
      (close-session session))))

(defun run-tests ()
  (let ((passed 0)
        (failed 0))
    (dolist (test (reverse *tests*))
      (handler-case
          (progn
            (funcall test)
            (incf passed)
            (format t "PASS ~A~%" test))
        (error (condition)
          (incf failed)
          (format t "FAIL ~A: ~A~%" test condition))))
    (format t "~A passed, ~A failed.~%" passed failed)
    (when (plusp failed)
      (error "The test suite has failures."))
    t))
