(in-package #:lispore.frontend)

(defconstant +default-width+ 80)
(defconstant +default-height+ 24)
(defconstant +frontend-poll-timeout+ 100)

(defun frontend-content-height (height)
  "Return the rows available to the shell below the status line."
  (max 1 (1- height)))

(defun status-line-text-for-width (width)
  "Pad or clip the status line to WIDTH columns."
  (if (>= width (length *default-status-line-text*))
      (concatenate 'string
                   *default-status-line-text*
                   (make-string (- width (length *default-status-line-text*))
                                :initial-element #\Space))
      (subseq *default-status-line-text* 0 width)))

(defun render-passthrough-status-line (width height)
  "Return ANSI output that draws the fixed passthrough status line."
  (format nil
          "~C7~C[1;~Dr~C[~D;1H~C[2K~C[30;42m~A~C[0m~C8"
          #\Escape
          #\Escape
          (frontend-content-height height)
          #\Escape
          height
          #\Escape
          #\Escape
          (status-line-text-for-width width)
          #\Escape
          #\Escape))

(defun render-passthrough-status-line-clear (height)
  "Return ANSI output that clears the passthrough status line."
  (format nil
          "~C7~C[1;~Dr~C[~D;1H~C[2K~C[0m~C8"
          #\Escape
          #\Escape
          height
          #\Escape
          height
          #\Escape
          #\Escape
          #\Escape))

(defun write-passthrough-status-line (output-fd width height)
  (when output-fd
    (write-fd output-fd
              (encode-utf8 (render-passthrough-status-line width height)))))

(defun clear-passthrough-status-line (output-fd height)
  (when output-fd
    (write-fd output-fd
              (encode-utf8 (render-passthrough-status-line-clear height)))))

(defun event-for-fd (events fd)
  (cdr (assoc fd events)))

(defun event-readable-p (events fd)
  (let ((revents (event-for-fd events fd)))
    (and revents
         (plusp (logand revents
                        (logior +pollin+ +pollerr+ +pollhup+ +pollnval+))))))

(defun terminal-dimensions (fd)
  (if fd
      (multiple-value-bind (width height)
          (lispore.platform:terminal-size fd)
        (values (if (and width (plusp width)) width +default-width+)
                (if (and height (plusp height)) height +default-height+)))
      (values +default-width+ +default-height+)))

(defun session-descriptors (session input-fd)
  (if input-fd
      (list (cons input-fd +pollin+)
            (cons (pty-master session) +pollin+))
      (list (cons (pty-master session) +pollin+))))

(defun handle-input (session input-fd)
  (multiple-value-bind (bytes eof-p)
      (read-fd input-fd :wait-p nil)
    (unless eof-p
      (when (plusp (length bytes))
        (write-input session bytes)))
    eof-p))

(defun drain-passthrough-output (session output-fd)
  (loop
    (multiple-value-bind (bytes eof-p)
        (read-output-bytes session :wait-p nil)
      (when (and bytes (plusp (length bytes)) output-fd)
        (write-fd output-fd bytes))
      (cond
        (eof-p (return t))
        ((or (null bytes) (zerop (length bytes)))
         (return nil))))))

(defun call-with-input-mode (input-fd function)
  (if input-fd
      (call-with-raw-terminal function :fd input-fd)
      (funcall function)))

(defun run-frontend-loop (session input-fd output-handler &key cycle-handler)
  "Run frontend events until SESSION reaches end of file."
  (let ((input-open-p (not (null input-fd))))
    (loop
      with session-eof-p = nil
      until session-eof-p
      for events = (progn
                     (when cycle-handler
                       (funcall cycle-handler))
                     (poll-fds (session-descriptors
                                session
                                (and input-open-p input-fd))
                               :timeout +frontend-poll-timeout+))
      do (when (event-readable-p events (pty-master session))
           (setf session-eof-p (funcall output-handler)))
         (when (and input-open-p
                    (not session-eof-p)
                    (event-readable-p events input-fd))
           (when (handle-input session input-fd)
             (setf input-open-p nil))))))

(defun handle-attachment-input (attachment input-fd)
  "Submit readable input for ATTACHMENT and return its EOF state."
  (multiple-value-bind (bytes eof-p)
      (read-fd input-fd :wait-p nil)
    (when (and (not eof-p) (plusp (length bytes)))
      ;; A rejected submission is intentionally invisible to the frontend.
      (submit-input attachment bytes))
    eof-p))

(defun ensure-attachment-mode (attachment mode)
  "Require ATTACHMENT to match the selected frontend MODE."
  (unless (eq mode (attachment-mode attachment))
    (error "The attachment mode does not match the frontend mode."))
  attachment)

(defun input-lines (text width)
  "Return TEXT wrapped into WIDTH-column lines."
  (let ((lines nil)
        (line nil)
        (column 0))
    (labels ((finish-line ()
               (push (coerce (nreverse line) 'string) lines)
               (setf line nil
                     column 0)))
      (loop for character across text
            do (if (char= character #\Newline)
                   (finish-line)
                   (progn
                     (when (= column width)
                       (finish-line))
                     (push character line)
                     (incf column))))
      (let ((full-line-at-end-p (= column width))
            (ends-with-newline-p (and (plusp (length text))
                                      (char= (char text (1- (length text)))
                                             #\Newline))))
        (finish-line)
        (when (and full-line-at-end-p
                   (not ends-with-newline-p))
          (push "" lines)))
      (nreverse lines))))

(defun input-cursor-row-column (text cursor width)
  "Return the wrapped row and column for CURSOR in TEXT."
  (let ((row 0)
        (column 0))
    (loop for index below cursor
          for character = (char text index)
          do (if (char= character #\Newline)
                 (setf row (1+ row)
                       column 0)
                 (progn
                   (when (= column width)
                     (setf row (1+ row)
                           column 0))
                   (incf column))))
    (when (= column width)
      (setf row (1+ row)
            column 0))
    (values row column)))

(defun fit-text-to-width (text width)
  "Pad or clip TEXT to WIDTH columns."
  (if (>= width (length text))
      (concatenate 'string text
                   (make-string (- width (length text))
                                :initial-element #\Space))
      (subseq text 0 width)))

(defun command-status-line (attachment width)
  "Return the command status line for ATTACHMENT."
  (fit-text-to-width
   (format nil " lispore | ~A | ~A "
           (session-id (attachment-session attachment))
           (string-downcase (symbol-name (execution-state attachment))))
   width))

(defun input-line-offset (cursor-row line-count total-lines)
  "Return a scroll offset that keeps the input cursor visible."
  (min (max 0 (- total-lines line-count))
       (max 0 (- cursor-row (1- line-count)))))

(defun render-command-frame (attachment)
  "Render shared output, the input area, and the status line."
  (let ((screen (attachment-screen attachment)))
    (when screen
      (multiple-value-bind (width height)
          (lispore.terminal:terminal-size screen)
        (let* ((text (input-draft attachment))
               (lines (input-lines text width))
               (available-height (max 1 (1- height)))
               (line-count (min available-height (length lines))))
          (set-status-line screen (command-status-line attachment width))
          (multiple-value-bind (cursor-row cursor-column)
              (input-cursor-row-column
               text
               (input-cursor attachment)
               width)
            (let* ((line-offset
                     (input-line-offset cursor-row line-count (length lines)))
                   (input-top (+ 1 (- available-height line-count))))
              (with-output-to-string (stream)
                (write-string (render-terminal screen) stream)
                (loop for index below line-count
                      for line = (nth (+ line-offset index) lines)
                      for row = (+ input-top index)
                      do (format stream "~C[~D;1H~C[2K~A"
                                 #\Escape row #\Escape line))
                (format stream "~C[~D;~DH~C[?25h"
                        #\Escape
                        (+ input-top
                           (max 0 (- cursor-row line-offset)))
                        (1+ cursor-column)
                        #\Escape)))))))))

(defun write-command-frame (attachment output-fd)
  "Write ATTACHMENT's command frame to OUTPUT-FD."
  (when output-fd
    (let ((frame (render-command-frame attachment)))
      (when frame
        (write-fd output-fd (encode-utf8 frame))))))

(defun drain-command-output (attachment)
  "Drain ATTACHMENT output and return EOF plus output state."
  (let ((output-p nil))
    (loop
      (multiple-value-bind (bytes eof-p)
          (read-attachment attachment :wait-p nil)
        (when (and bytes (plusp (length bytes)))
          (setf output-p t))
        (cond
          (eof-p (return (values t output-p)))
          ((or (null bytes) (zerop (length bytes)))
           (return (values nil output-p))))))))

(defun handle-command-enter (attachment editor text)
  "Apply Enter rules to one ATTACHMENT draft."
  (case (input-completeness text)
    (:incomplete
     (input-editor-paste editor (string #\Newline)))
    ((:complete :error)
     (when (= (input-cursor attachment) (length text))
       (submit-command attachment text (input-language text))))))

(defun handle-command-input (attachment bytes)
  "Apply input BYTES and return whether the frontend should close."
  (let ((editor (attachment-input-editor attachment)))
    (input-editor-set-history editor (input-history attachment))
    (let ((byte (make-array 1 :element-type '(unsigned-byte 8)))
          (close-p nil))
      (loop for index below (length bytes)
            while (not close-p)
            do (setf (aref byte 0) (aref bytes index))
               (dolist (event (input-editor-feed editor byte))
                 (case (input-event-type event)
                   (:enter
                    (handle-command-enter attachment editor
                                           (input-event-text event)))
                   (:interrupt
                    (interrupt-execution attachment)
                    (input-editor-clear editor))
                   (:eof
                    (setf close-p t)))))
      close-p)))

(defun write-bracketed-paste-mode (output-fd enabled-p)
  "Enable or disable bracketed paste on OUTPUT-FD."
  (when output-fd
    (write-fd output-fd
              (encode-utf8
               (if enabled-p
                   (format nil "~C[?2004h" #\Escape)
                   (format nil "~C[?2004l" #\Escape))))))

(defun run-command (&key
                         (attachment nil)
                         (input-fd 0)
                         (output-fd 1))
  "Run the command frontend for ATTACHMENT."
  (unless attachment
    (error "The command frontend requires a managed attachment."))
  (ensure-attachment-mode attachment :command)
  (unwind-protect
       (progn
         (write-bracketed-paste-mode output-fd t)
         (write-command-frame attachment output-fd)
         (call-with-input-mode
          input-fd
          (lambda ()
            (let ((input-open-p (not (null input-fd)))
                  (finished-p nil)
                  (last-draft nil)
                  (last-state nil)
                  (last-cursor nil))
              (loop until finished-p
                    do (multiple-value-bind (session-eof-p output-p)
                           (drain-command-output attachment)
                         (let ((draft (input-draft attachment))
                               (state (execution-state attachment))
                               (cursor (input-cursor attachment)))
                           (when (or output-p
                                     (not (equal draft last-draft))
                                     (not (eq state last-state))
                                     (not (eql cursor last-cursor)))
                             (write-command-frame attachment output-fd)
                             (setf last-draft draft
                                   last-state state
                                   last-cursor cursor)))
                         (when session-eof-p
                           (setf finished-p t))
                         (unless finished-p
                           (if input-open-p
                               (let ((events
                                       (poll-fds
                                        (list (cons input-fd +pollin+))
                                        :timeout +frontend-poll-timeout+)))
                                 (when (event-readable-p events input-fd)
                                   (multiple-value-bind (bytes eof-p)
                                       (read-fd input-fd :wait-p nil)
                                     (if eof-p
                                         (setf finished-p t
                                               input-open-p nil)
                                         (when (and bytes
                                                    (plusp (length bytes)))
                                           (setf finished-p
                                                 (handle-command-input
                                                  attachment bytes)))))))
                               (sleep 0.01)))))))))
    ;; A disconnected frontend must still detach its session.
    (ignore-errors (write-bracketed-paste-mode output-fd nil))
    (ignore-errors (detach attachment)))
  nil)

(defun run-attachment-loop (attachment input-fd output-handler)
  "Run an attachment until it detaches or its session terminates."
  (let ((input-open-p (not (null input-fd))))
    (loop
      with finished-p = nil
      until finished-p
      do (setf finished-p (funcall output-handler))
         (unless finished-p
           (let ((events
                   (cond
                     (input-open-p
                      (poll-fds (list (cons input-fd +pollin+))
                                :timeout +frontend-poll-timeout+))
                     (t
                      (sleep 0.01)
                      nil))))
             (when (and input-open-p
                        (event-readable-p events input-fd)
                        (handle-attachment-input attachment input-fd))
               (detach attachment)
               (setf finished-p t)))))))

(defun run-passthrough (&key
                             (session nil)
                             (attachment nil)
                             (input-fd 0)
                             (output-fd 1))
  "Run a passthrough frontend until SESSION exits."
  (when (and session attachment)
    (error "A frontend cannot receive both a session and an attachment."))
  (if attachment
      (run-attached-passthrough attachment input-fd output-fd)
      (let ((owned-session-p (null session))
            (session (or session (start-shell)))
            (width nil)
            (height nil))
        (unwind-protect
            (progn
              (multiple-value-setq (width height)
                (terminal-dimensions input-fd))
              (resize-session session width (frontend-content-height height))
              (write-passthrough-status-line output-fd width height)
              (call-with-input-mode
               input-fd
               (lambda ()
                 (run-frontend-loop
                  session
                  input-fd
                  (lambda ()
                    (let ((eof-p (drain-passthrough-output session output-fd)))
                      (unless eof-p
                        (write-passthrough-status-line output-fd width height))
                      eof-p))
                  :cycle-handler
                  (lambda ()
                    (when input-fd
                      (multiple-value-bind (new-width new-height)
                          (terminal-dimensions input-fd)
                        (unless (and (= new-width width)
                                     (= new-height height))
                          (setf width new-width
                                height new-height)
                          (resize-session
                           session
                           width
                           (frontend-content-height height))
                          (write-passthrough-status-line
                           output-fd
                           width
                           height))))))))
              (wait-for-session session))
          (when height
            (clear-passthrough-status-line output-fd height))
          (when owned-session-p
            (close-session session))))))

(defun drain-attached-passthrough-output (attachment output-fd)
  "Forward available bytes from ATTACHMENT and report whether output arrived."
  (let ((output-p nil))
    (loop
      (multiple-value-bind (bytes eof-p)
          (read-attachment attachment :wait-p nil)
        (when (and bytes (plusp (length bytes)))
          (setf output-p t)
          (when output-fd
            (write-fd output-fd bytes)))
        (cond
          (eof-p (return (values t output-p)))
          ((or (null bytes) (zerop (length bytes)))
           (return (values nil output-p)))
          (t nil))))))

(defun write-terminal (terminal output-fd)
  "Write TERMINAL's screen to OUTPUT-FD."
  (when output-fd
    (write-fd output-fd (encode-utf8 (render-terminal terminal)))))

(defun run-attached-passthrough (attachment input-fd output-fd)
  "Run a passthrough frontend from ATTACHMENT."
  (ensure-attachment-mode attachment :passthrough)
  (let ((terminal (attachment-start-screen attachment)))
    (multiple-value-bind (width height)
        (lispore.terminal:terminal-size terminal)
      (unwind-protect
          (progn
            (write-terminal terminal output-fd)
            ;; Restore the reserved scroll region after rendering the screen.
            (write-passthrough-status-line output-fd width height)
            (call-with-input-mode
             input-fd
             (lambda ()
               (run-attachment-loop
                attachment
                input-fd
                (lambda ()
                  (multiple-value-bind (eof-p output-p)
                      (drain-attached-passthrough-output attachment output-fd)
                    (when (and output-p (not eof-p))
                      (write-passthrough-status-line
                       output-fd
                       width
                       height))
                    eof-p))))))
        (ignore-errors (clear-passthrough-status-line output-fd height))
        (ignore-errors (detach attachment))))
    nil))

(defun interactive-shell (&key
                                (attachment nil)
                                (input-fd 0)
                                (output-fd 1))
  "Run the command frontend for ATTACHMENT."
  (run-command :attachment attachment
               :input-fd input-fd
               :output-fd output-fd))
