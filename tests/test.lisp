(in-package #:lispore.tests)

(defvar *tests* nil)

(defmacro deftest (name () &body body)
  `(progn
     (defun ,name () ,@body)
     (pushnew ',name *tests*)))

(defun check (condition format-control &rest format-arguments)
  (unless condition
    (error (apply #'format nil format-control format-arguments))))

(defun screen-has-text-p (terminal text)
  (some (lambda (line) (search text line))
        (screen-lines terminal)))

(defun wait-until (predicate &key (attempts 100) (delay 0.05))
  (loop repeat attempts
        when (funcall predicate)
          do (return t)
        do (sleep delay)
        finally (return (funcall predicate))))

(defun attachment-output-has-text-p (attachment text)
  (let ((output ""))
    (loop repeat 100
          do (multiple-value-bind (bytes eof-p)
                 (read-attachment attachment :wait-p nil)
               (when (and bytes (plusp (length bytes)))
                 (setf output
                       (concatenate 'string
                                    output
                                    (map 'string #'code-char bytes)))
                 (when (search text output)
                   (return-from attachment-output-has-text-p t)))
               (when eof-p
                 (return nil))
               (sleep 0.01)))
    nil))

(defun drain-attachment (attachment)
  "Remove all currently buffered output from ATTACHMENT."
  (loop
    for bytes = (read-attachment attachment :wait-p nil)
    while (and bytes (plusp (length bytes)))))

(deftest detached-session-restores-retained-screen ()
  (let ((manager (make-session-manager :retention-seconds 5)))
    (unwind-protect
        (let* ((session-id (start-session manager
                                          :shell "/bin/sh"
                                          :width 20
                                          :height 4))
               (session (lookup-session manager session-id)))
          (multiple-value-bind (attachment screen)
              (restore-session manager session-id)
            (check attachment
                   "The session does not accept its first attachment.")
            (check (not (screen-has-text-p screen "detach-marker"))
                   "The first attachment returns stale screen data.")
            (set-input-draft attachment
                             (format nil "printf 'detach-marker\\n'; sleep 1~%"))
            (check (submit-input attachment)
                   "The first input submission is rejected.")
            (check (wait-until
                    (lambda ()
                      (screen-has-text-p (attachment-screen attachment)
                                         "detach-marker")))
                   "The session does not retain shell output.")
            (check (detach attachment)
                   "The frontend cannot detach from the session.")
            (check (session-running-p session)
                   "Detachment terminates the shell session.")
            (multiple-value-bind (restored screen)
                (restore-session manager session-id)
              (check restored
                     "The running session cannot be restored.")
              (check (screen-has-text-p screen "detach-marker")
                     "Restoration loses the retained screen.")
              (set-input-draft restored
                               (format nil "printf 'restore-marker\\n'; exit~%"))
              (check (submit-input restored)
                     "The restored frontend cannot submit input.")
              (check (wait-until
                      (lambda ()
                        (not (session-running-p session))))
                     "The restored shell session does not terminate.")
              (check (not (attachment-attached-p restored))
                     "Session termination leaves the attachment connected.")
              (check (null (restore-session manager session-id))
                     "The terminated session accepts a new attachment.")
              (check (screen-has-text-p (attachment-screen restored)
                                        "restore-marker")
                     "The attached frontend loses the final screen."))))
      (close-session-manager manager))))

(deftest managed-session-broadcasts-output-to-every-attachment ()
  (let ((manager (make-session-manager :retention-seconds 5)))
    (unwind-protect
        (let ((session-id (start-session manager
                                         :shell "/bin/sh"
                                         :width 20
                                         :height 4)))
          (let ((first (attach-session manager session-id))
                (second (attach-session manager session-id)))
            (check (and first second)
                   "The session does not accept two attachments.")
            (set-input-draft first
                             (format nil "printf 'broadcast-marker\\n'; sleep 1~%"))
            (check (submit-input first)
                   "The attached frontend cannot submit input.")
            (check (and (attachment-output-has-text-p first
                                                      "broadcast-marker")
                        (attachment-output-has-text-p second
                                                      "broadcast-marker"))
                   "The session does not broadcast output to every attachment.")
            (check (and (attachment-attached-p first)
                        (attachment-attached-p second))
                   "A healthy attachment disconnects unexpectedly.")))
      (close-session-manager manager))))

(deftest attachments-keep-private-input-drafts ()
  (let ((manager (make-session-manager :retention-seconds 5)))
    (unwind-protect
        (let* ((session-id (start-session manager :shell "/bin/sh"))
               (first (attach-session manager session-id))
               (second (attach-session manager session-id)))
          (set-input-draft first "printf 'draft-marker\\n'")
          (set-input-draft second "printf 'other-draft\\n'")
          (check (and (string= "printf 'draft-marker\\n'"
                               (input-draft first))
                      (string= "printf 'other-draft\\n'"
                               (input-draft second)))
                 "Attachments do not keep private input drafts.")
          (check (submit-input first)
                 "The draft submission is rejected.")
          (check (string= "" (input-draft first))
                 "A submitted draft remains in the input box.")
          (check (string= "printf 'other-draft\\n'"
                          (input-draft second))
                 "One attachment changes another draft.")
          (check (detach second)
                 "The second attachment cannot detach.")
          (check (not (submit-input second))
                 "A detached frontend submits input.")
          (check (string= "printf 'other-draft\\n'"
                          (input-draft second))
                 "A rejected draft does not stay in its input box."))
      (close-session-manager manager))))

(deftest slow-attachment-disconnects-after-buffer-overflow ()
  (let ((manager (make-session-manager
                  :retention-seconds 5
                  :max-buffer-bytes 8192)))
    (unwind-protect
        (let* ((session-id (start-session manager :shell "/bin/sh"))
               (slow (attach-session manager session-id))
               (healthy (attach-session manager session-id)))
          (set-input-draft healthy
                           (format nil
                                   "i=0; while [ $i -lt 100 ]; do printf 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'; i=$((i+1)); done; sleep 1~%"))
          (check (submit-input healthy)
                 "The overflow test input is rejected.")
          (check (wait-until
                  (lambda ()
                    (read-attachment healthy :wait-p nil)
                    (not (attachment-attached-p slow)))
                  :attempts 300
                  :delay 0.01)
                 "A slow attachment survives buffer overflow.")
          (check (attachment-attached-p healthy)
                 "A healthy attachment disconnects with the slow one."))
      (close-session-manager manager))))

(deftest blocked-attachment-reader-wakes-after-buffer-overflow ()
  (let ((manager (make-session-manager
                  :retention-seconds 5
                  :max-buffer-bytes 32)))
    (unwind-protect
        (let* ((session-id (start-session manager :shell "/bin/sh"))
               (slow (attach-session manager session-id))
               (writer (attach-session manager session-id))
               (result nil)
               (reader nil)
               (ready-lock (make-lock "reader readiness"))
               (ready-condition (make-condition-variable :name "reader readiness"))
               (ready-p nil))
          (drain-attachment slow)
          (setf reader
                (make-thread
                 (lambda ()
                   (with-lock-held (ready-lock)
                     (setf ready-p t)
                     (condition-notify ready-condition))
                   (loop
                     (multiple-value-bind (bytes eof-p)
                         (read-attachment slow)
                       (declare (ignore bytes))
                       (when eof-p
                         (setf result (list nil t))
                         (return)))))))
          (with-lock-held (ready-lock)
            (loop until ready-p
                  do (condition-wait ready-condition ready-lock)))
          (set-input-draft writer
                           (format nil "printf 'overflow-overflow-overflow-overflow'; sleep 1~%"))
          (check (submit-input writer)
                 "The overflow wakeup input is rejected.")
          (check (wait-until (lambda () (not (attachment-attached-p slow))))
                 "Buffer overflow does not disconnect the slow attachment.")
          (join-thread reader)
          (check (and result (null (first result)) (second result))
                 "A blocked reader does not wake after disconnection."))
      (close-session-manager manager))))

(deftest terminated-session-retains-screen-for-a-fixed-time ()
  (let ((manager (make-session-manager :retention-seconds 0.2)))
    (unwind-protect
        (let* ((session-id (start-session manager :shell "/bin/sh"))
               (session (lookup-session manager session-id))
               (attachment (attach-session manager session-id)))
          (set-input-draft attachment
                           (format nil "printf 'final-marker\\n'; exit~%"))
          (check (submit-input attachment)
                 "The final-screen input is rejected.")
          (check (wait-until (lambda () (not (session-running-p session))))
                 "The session does not terminate naturally.")
          (check (lookup-session manager session-id)
                 "The manager drops the retained session too early.")
          (check (screen-has-text-p (retained-screen session) "final-marker")
                 "The retained screen loses final output.")
                 (check (wait-until (lambda () (null (lookup-session manager session-id)))
                             :attempts 100
                             :delay 0.01)
                 "The manager keeps final screen data beyond its retention time.")
          (check (null (attachment-screen attachment))
                 "An attachment keeps final screen data beyond retention."))
      (close-session-manager manager))))

(deftest explicit-termination-stops-managed-session ()
  (let ((manager (make-session-manager :retention-seconds 5)))
    (unwind-protect
        (let* ((session-id (start-session manager :shell "/bin/sh"))
               (session (lookup-session manager session-id))
               (attachment (attach-session manager session-id)))
          (check (terminate-session manager session-id)
                 "Explicit termination does not return success.")
          (check (not (session-running-p session))
                 "Explicit termination leaves the session running.")
          (check (not (attachment-attached-p attachment))
                 "Explicit termination leaves the attachment connected.")
          (check (null (restore-session manager session-id))
                 "Explicit termination allows restoration."))
      (close-session-manager manager))))

(deftest emulated-frontend-can-run-from-an-attachment ()
  (let ((manager (make-session-manager :retention-seconds 5)))
    (unwind-protect
        (let* ((session-id (start-session manager :shell "/bin/sh"))
               (session (lookup-session manager session-id))
               (attachment (attach-session manager session-id))
               (result nil)
               (thread
                 (make-thread
                  (lambda ()
                    (multiple-value-bind (status terminal)
                        (run-emulated :attachment attachment
                                      :input-fd nil
                                      :output-fd nil)
                      (setf result (list status terminal)))))))
          (sleep 0.05)
          (set-input-draft attachment
                           (format nil "printf 'frontend-marker\\n'; sleep 1~%"))
          (check (submit-input attachment)
                 "The attached frontend cannot submit input.")
          (check (wait-until
                  (lambda ()
                    (screen-has-text-p (attachment-screen attachment)
                                       "frontend-marker")))
                 "The attached frontend does not receive output.")
          (check (detach attachment)
                 "The attached frontend cannot detach.")
          (join-thread thread)
          (check (and result
                      (screen-has-text-p (second result) "frontend-marker"))
                 "The frontend does not retain its final screen after detach.")
          (check (session-running-p session)
                 "Frontend detachment terminates the shell session."))
      (close-session-manager manager))))

(deftest passthrough-frontend-can-run-from-an-attachment ()
  (let ((manager (make-session-manager :retention-seconds 5)))
    (unwind-protect
        (let* ((session-id (start-session manager :shell "/bin/sh"))
               (session (lookup-session manager session-id))
               (attachment (attach-session manager session-id :mode :passthrough))
               (finished-p nil)
               (thread
                 (make-thread
                  (lambda ()
                    (run-passthrough :attachment attachment
                                     :input-fd nil
                                     :output-fd nil)
                    (setf finished-p t)))))
          (set-input-draft attachment
                           (format nil "printf 'passthrough-marker\\n'; sleep 1~%"))
          (check (submit-input attachment)
                 "The passthrough frontend cannot submit input.")
          (check (wait-until
                  (lambda ()
                    (screen-has-text-p (attachment-screen attachment)
                                       "passthrough-marker")))
                 "The passthrough attachment does not receive output.")
          (check (detach attachment)
                 "The passthrough frontend cannot detach.")
          (join-thread thread)
          (check finished-p
                 "The passthrough frontend does not stop after detach.")
          (check (session-running-p session)
                 "Passthrough detachment terminates the shell session."))
      (close-session-manager manager))))

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

(deftest terminal-reserves-a-colored-status-line ()
  (let ((terminal (make-terminal-emulator :width 20 :height 4)))
    (set-status-line terminal " lispore | shell ")
    (feed-terminal terminal (format nil "content~C[4;1Hbottom"
                                    #\Escape))
    (let ((status (cell-at terminal 4 1)))
      (check (string= " lispore | shell    "
                      (fourth (screen-lines terminal)))
             "The terminal does not render the status line.")
      (check (and (member 30 (screen-cell-style status))
                  (member 42 (screen-cell-style status)))
             "The status line does not use black text on green."))
    (check (search "bottom" (third (screen-lines terminal)))
           "Shell output overwrites the status line.")))

(deftest terminal-preserves-status-line-after-resize-and-reset ()
  (let ((terminal (make-terminal-emulator :width 20 :height 4)))
    (set-status-line terminal " lispore | shell ")
    (resize-terminal terminal 12 3)
    (feed-terminal terminal (format nil "~Cc" #\Escape))
    (check (string= " lispore | s" (third (screen-lines terminal)))
           "The status line does not survive terminal reset.")
    (check (and (= 12 (length (third (screen-lines terminal))))
                (some (lambda (line) (search " lispore | " line))
                      (screen-lines terminal)))
           "The resized status line has an unexpected width.")))

(deftest passthrough-status-line-uses-fixed-ansi-layout ()
  (let ((output (lispore.frontend::render-passthrough-status-line 20 4)))
    (check (search (format nil "~C[1;3r" #\Escape) output)
           "Passthrough mode does not reserve one row.")
    (check (search (format nil "~C[30;42m" #\Escape) output)
           "Passthrough mode does not set status colors.")
    (check (search " lispore | shell " output)
           "Passthrough mode does not render status text.")))

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

(deftest emulated-frontend-reserves-status-line-height ()
  (let ((session (start-shell :shell "/bin/sh" :width 20 :height 4))
        (terminal (make-terminal-emulator :width 20 :height 4)))
    (unwind-protect
        (progn
          (write-input session (format nil "stty size; exit~%"))
          (run-emulated :session session
                        :terminal terminal
                        :input-fd nil
                        :output-fd nil)
          (check (some (lambda (line) (search "3 20" line))
                       (screen-lines terminal))
                 "The emulated frontend does not reserve status height.")
          (check (string= " lispore | shell    "
                          (first (last (screen-lines terminal))))
                 "The emulated frontend loses its status line."))
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
