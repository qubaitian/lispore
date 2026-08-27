(in-package #:lispore.pty)

(defclass shell-session ()
  ((master
    :initarg :master
    :reader pty-master)
   (process-id
    :initarg :process-id
    :reader session-process-id)
   (open-p
    :initform t
    :accessor session-open-p)
   (eof-p
    :initform nil
    :accessor session-eof-p)
   (pending-bytes
    :initform nil
    :accessor session-pending-bytes)
   (reaped-p
    :initform nil
    :accessor session-reaped-p)
   (status
    :initform nil
    :accessor session-status)))

(defun current-shell ()
  (or (uiop:getenv "SHELL") "/bin/sh"))

(defun start-shell (&key (shell (current-shell)) (width 80) (height 24))
  "Start the current shell inside a PTY."
  (multiple-value-bind (master process-id)
      (start-pty shell width height)
    (make-instance 'shell-session
                   :master master
                   :process-id process-id)))

(defun ensure-open (session)
  (unless (session-open-p session)
    (error "The shell session is closed."))
  session)

(defun remember-process-status (session status)
  (when status
    (setf (session-status session) status
          (session-reaped-p session) t)))

(defun read-output-bytes (session &key (max-bytes 4096) (wait-p t))
  "Read PTY bytes and return bytes plus an end-of-file flag."
  (ensure-open session)
  (multiple-value-bind (bytes eof-p)
      (read-fd (pty-master session) :max-bytes max-bytes :wait-p wait-p)
    (when eof-p
      (setf (session-eof-p session) t)
      (remember-process-status
       session
       (wait-process (session-process-id session) :no-hang-p t)))
    (values (unless eof-p bytes) eof-p)))

(defun read-output (session &key (max-bytes 4096) (wait-p t))
  "Read decoded UTF-8 terminal text from SESSION."
  (ensure-open session)
  (loop
    (multiple-value-bind (bytes eof-p)
        (read-output-bytes session :max-bytes max-bytes :wait-p wait-p)
      (cond
        (eof-p (return nil))
        ((plusp (length bytes))
         (multiple-value-bind (text pending)
             (decode-utf8-chunk bytes (session-pending-bytes session))
           (setf (session-pending-bytes session) pending)
           (if (plusp (length text))
               (return text)
               (unless wait-p
                 (return "")))))
        (t (return ""))))))

(defun valid-byte-vector-p (bytes)
  (and (vectorp bytes)
       (every (lambda (byte)
               (and (integerp byte) (<= 0 byte 255)))
              bytes)))

(defun write-input (session input)
  "Write UTF-8 text or raw octets into SESSION."
  (ensure-open session)
  (let ((bytes (etypecase input
                 (string (encode-utf8 input))
                 (vector
                  (unless (valid-byte-vector-p input)
                    (error "Input vector must contain octets."))
                  input))))
    (write-fd (pty-master session) bytes)))

(defun resize-session (session width height)
  "Resize SESSION using character-cell WIDTH and HEIGHT."
  (ensure-open session)
  (resize-pty (pty-master session) width height))

(defun wait-for-session (session)
  "Wait for SESSION's shell and return its exit status."
  (unless (session-reaped-p session)
    (remember-process-status
     session
     (wait-process (session-process-id session))))
  (session-status session))

(defun close-session (session)
  "Close SESSION and reap its shell process."
  (when (session-open-p session)
    (setf (session-open-p session) nil
          (session-eof-p session) t)
    (unwind-protect
        (progn
          (ignore-errors (close-pty (pty-master session)))
          (ignore-errors (terminate-process (session-process-id session)))
          (unless (session-reaped-p session)
            (remember-process-status
             session
             (wait-process (session-process-id session)))))
      (setf (session-open-p session) nil)))
  t)
