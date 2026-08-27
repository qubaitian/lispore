(in-package #:lispore.frontend)

(defconstant +default-width+ 80)
(defconstant +default-height+ 24)

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

(defun run-frontend-loop (session input-fd output-handler)
  "Run frontend events until SESSION reaches end of file."
  (let ((input-open-p (not (null input-fd))))
    (loop
      with session-eof-p = nil
      until session-eof-p
      for events = (poll-fds (session-descriptors
                              session
                              (and input-open-p input-fd)))
      do (when (event-readable-p events (pty-master session))
           (setf session-eof-p (funcall output-handler)))
         (when (and input-open-p
                    (not session-eof-p)
                    (event-readable-p events input-fd))
           (when (handle-input session input-fd)
             (setf input-open-p nil))))))

(defun run-passthrough (&key (session nil) (input-fd 0) (output-fd 1))
  "Run a passthrough frontend until SESSION exits."
  (let ((owned-session-p (null session))
        (session (or session (start-shell))))
    (unwind-protect
        (progn
          (multiple-value-bind (width height)
              (terminal-dimensions input-fd)
            (resize-session session width height))
          (call-with-input-mode
           input-fd
           (lambda ()
             (run-frontend-loop
              session
              input-fd
              (lambda ()
                (drain-passthrough-output session output-fd)))))
          (wait-for-session session))
      (when owned-session-p
        (close-session session)))))

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

(defun run-emulated (&key (session nil)
                          (terminal nil)
                          (input-fd 0)
                          (output-fd 1))
  "Run an emulated frontend until SESSION exits."
  (let* ((owned-session-p (null session))
         (session (or session (start-shell)))
         (terminal (or terminal
                       (multiple-value-bind (width height)
                           (terminal-dimensions input-fd)
                         (make-terminal-emulator :width width :height height)))))
    (unwind-protect
        (progn
          (multiple-value-bind (width height)
              (lispore.terminal:terminal-size terminal)
            (resize-session session width height))
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
                  eof-p)))))
          (values (wait-for-session session) terminal))
      (when owned-session-p
        (close-session session)))))

(defun interactive-shell (&key (mode :passthrough)
                                (session nil)
                                (input-fd 0)
                                (output-fd 1)
                                (terminal nil))
  "Run a blocking shell frontend in MODE."
  (ecase mode
    (:passthrough
     (run-passthrough :session session
                      :input-fd input-fd
                      :output-fd output-fd))
    (:emulated
     (run-emulated :session session
                   :terminal terminal
                   :input-fd input-fd
                   :output-fd output-fd))))
