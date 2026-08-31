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

(defun get-current-shell ()
  (or (uiop:getenv "SHELL") "/bin/sh"))

(defun new-shell-session (&key (shell (get-current-shell)) (width 80) (height 24))
  "Start the current shell inside a PTY."
  (multiple-value-bind (master process-id)
      (new-pty shell width height)
    (make-instance 'shell-session
                   :master master
                   :process-id process-id)))

(defun get-open-shell-session (session)
  (unless (session-open-p session)
    (error "The shell session is closed."))
  session)

(defun set-shell-process-status (session status)
  (when status
    (setf (session-status session) status
          (session-reaped-p session) t)))

(defun get-shell-output-bytes (session &key (max-bytes 4096) (wait-p t))
  "Read PTY bytes and return bytes plus an end-of-file flag."
  (get-open-shell-session session)
  (multiple-value-bind (bytes eof-p)
      (get-fd (pty-master session) :max-bytes max-bytes :wait-p wait-p)
    (when eof-p
      (setf (session-eof-p session) t)
      (set-shell-process-status
       session
       (get-process-status (session-process-id session) :no-hang-p t)))
    (values (unless eof-p bytes) eof-p)))

(defun get-shell-output (session &key (max-bytes 4096) (wait-p t))
  "Read decoded UTF-8 terminal text from SESSION."
  (get-open-shell-session session)
  (loop
    (multiple-value-bind (bytes eof-p)
        (get-shell-output-bytes session :max-bytes max-bytes :wait-p wait-p)
      (cond
        (eof-p (return nil))
        ((plusp (length bytes))
         (multiple-value-bind (text pending)
             (get-utf8-chunk bytes (session-pending-bytes session))
           (setf (session-pending-bytes session) pending)
           (if (plusp (length text))
               (return text)
               (unless wait-p
                 (return "")))))
        (t (return ""))))))

(defun get-valid-byte-vector-p (bytes)
  (and (vectorp bytes)
       (every (lambda (byte)
               (and (integerp byte) (<= 0 byte 255)))
              bytes)))

(defun set-shell-input (session input)
  "Write UTF-8 text or raw octets into SESSION."
  (get-open-shell-session session)
  (let ((bytes (etypecase input
                 (string (get-utf8 input))
                 (vector
                  (unless (get-valid-byte-vector-p input)
                    (error "Input vector must contain octets."))
                  input))))
    (set-fd (pty-master session) bytes)))

(defun set-shell-size (session width height)
  "Resize SESSION using character-cell WIDTH and HEIGHT."
  (get-open-shell-session session)
  (set-pty-size (pty-master session) width height))

(defun get-shell-process-status (session)
  "Wait for SESSION's shell and return its exit status."
  (unless (session-reaped-p session)
    (set-shell-process-status
     session
     (get-process-status (session-process-id session))))
  (session-status session))

(defun del-shell-session (session)
  "Close SESSION and reap its shell process."
  (when (session-open-p session)
    (setf (session-open-p session) nil
          (session-eof-p session) t)
    (unwind-protect
        (progn
          (ignore-errors (del-pty (pty-master session)))
          (ignore-errors (del-process (session-process-id session)))
          (unless (session-reaped-p session)
            (set-shell-process-status
             session
             (get-process-status (session-process-id session)))))
      (setf (session-open-p session) nil)))
  t)
