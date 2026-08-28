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

(defun drain-emulated-output (session terminal)
  (let ((changed-p nil))
    (loop
      for text = (read-output session :wait-p nil)
      do (cond
           ((null text) (return (values changed-p t)))
           ((zerop (length text)) (return (values changed-p nil)))
           (t
            (feed-terminal terminal text)
            (setf changed-p t))))))

(defun write-terminal (terminal output-fd)
  (when output-fd
    (write-fd output-fd (encode-utf8 (render-terminal terminal)))))

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

(defun make-attached-emulated-output-handler
    (attachment terminal output-fd initial-pending-bytes)
  "Create a handler that feeds ATTACHMENT bytes into TERMINAL."
  (let ((pending-bytes initial-pending-bytes))
    (lambda ()
      (let ((changed-p nil))
        (loop
          (multiple-value-bind (bytes eof-p)
              (read-attachment attachment :wait-p nil)
            (cond
              (eof-p
               (when changed-p
                 (write-terminal terminal output-fd))
               (return t))
              ((or (null bytes) (zerop (length bytes)))
               (when changed-p
                 (write-terminal terminal output-fd))
               (return nil))
              (t
               (multiple-value-bind (text remaining)
                   (decode-utf8-chunk bytes pending-bytes)
                 (setf pending-bytes remaining)
                 (when (plusp (length text))
                   (feed-terminal terminal text)
                   (setf changed-p t)))))))))))

(defun run-attached-passthrough (attachment input-fd output-fd)
  "Run a passthrough frontend from ATTACHMENT."
  (ensure-attachment-mode attachment :passthrough)
  (multiple-value-bind (terminal ignored-pending-bytes)
      (attachment-start-screen attachment)
    (declare (ignore ignored-pending-bytes))
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

(defun run-attached-emulated (attachment input-fd output-fd)
  "Run an emulated frontend from ATTACHMENT."
  (ensure-attachment-mode attachment :emulated)
  (multiple-value-bind (terminal pending-bytes)
      (attachment-start-screen attachment)
    (unwind-protect
        (progn
          (set-status-line terminal *default-status-line-text*)
          (write-terminal terminal output-fd)
          (call-with-input-mode
           input-fd
           (lambda ()
             (run-attachment-loop
              attachment
              input-fd
              (make-attached-emulated-output-handler
               attachment
               terminal
               output-fd
               pending-bytes)))))
      (ignore-errors (detach attachment)))
    (values nil terminal)))

(defun run-emulated (&key (session nil)
                          (terminal nil)
                          (input-fd 0)
                          (output-fd 1)
                          (attachment nil))
  "Run an emulated frontend until SESSION exits."
  (when (and session attachment)
    (error "A frontend cannot receive both a session and an attachment."))
  (when (and terminal attachment)
    (error "An attachment must restore its own retained terminal screen."))
  (if attachment
      (run-attached-emulated attachment input-fd output-fd)
      (let* ((owned-session-p (null session))
             (session (or session (start-shell)))
             (terminal (or terminal
                           (multiple-value-bind (width height)
                               (terminal-dimensions input-fd)
                             (make-terminal-emulator
                              :width width
                              :height height))))
             (width nil)
             (height nil))
        (unwind-protect
            (progn
              (set-status-line terminal *default-status-line-text*)
              (multiple-value-setq (width height)
                (lispore.terminal:terminal-size terminal))
              (resize-session session width (frontend-content-height height))
              (write-terminal terminal output-fd)
              (call-with-input-mode
               input-fd
               (lambda ()
                 (run-frontend-loop
                  session
                  input-fd
                  (lambda ()
                    (multiple-value-bind (changed-p eof-p)
                        (drain-emulated-output session terminal)
                      (when changed-p
                        (write-terminal terminal output-fd))
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
                          (resize-terminal terminal width height)
                          (resize-session
                           session
                           width
                           (frontend-content-height height))
                          (write-terminal terminal output-fd))))))))
              (values (wait-for-session session) terminal))
          (when owned-session-p
            (close-session session))))))

(defun interactive-shell (&key (mode :passthrough)
                                (session nil)
                                (attachment nil)
                                (input-fd 0)
                                (output-fd 1)
                                (terminal nil))
  "Run a blocking shell frontend in MODE."
  (ecase mode
    (:passthrough
     (run-passthrough :session session
                      :attachment attachment
                      :input-fd input-fd
                      :output-fd output-fd))
    (:emulated
     (run-emulated :session session
                   :attachment attachment
                   :terminal terminal
                   :input-fd input-fd
                   :output-fd output-fd))))
