(in-package #:lispore.logging)

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(defclass diagnostic-logger ()
  ((stream
    :initarg :stream
    :reader diagnostic-logger-stream)
   (broadcast-function
    :initarg :broadcast-function
    :reader diagnostic-logger-broadcast-function)
   (lock
    :initform (make-lock "lispore diagnostic logger")
    :reader diagnostic-logger-lock)
   (condition
    :initform (make-condition-variable :name "lispore diagnostic logger")
    :reader diagnostic-logger-condition)
   (pending-records
    :initform nil
    :accessor diagnostic-logger-pending-records)
   (closed-p
    :initform nil
    :accessor diagnostic-logger-closed-p)
   (worker-thread
    :initform nil
    :accessor diagnostic-logger-worker-thread)
   (stream-closed-p
    :initform nil
    :accessor diagnostic-logger-stream-closed-p))
  (:documentation "Write complete diagnostic records and broadcast them."))

(defun get-diagnostic-process-id ()
  "Return the current process identifier when SBCL provides one."
  #+sbcl
  (sb-posix:getpid)
  #-sbcl
  0)

(defun get-diagnostic-thread-name ()
  "Return the current thread name without masking the diagnostic event."
  (handler-case
      (or (thread-name (current-thread)) "anonymous")
    (error () "anonymous")))

(defun get-diagnostic-timestamp ()
  "Return the local diagnostic timestamp in sortable text."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time))
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D"
            year month day hour minute second)))

(defun set-diagnostic-section (stream name value)
  "Write VALUE between named section markers."
  (format stream "~A-begin~%" name)
  (write-string (princ-to-string value) stream)
  (terpri stream)
  (format stream "~A-end~%" name))

(defun set-diagnostic-condition (stream condition)
  "Write CONDITION and its SBCL backtrace."
  (set-diagnostic-section stream "condition" condition)
  #+sbcl
  (handler-case
      (progn
        (format stream "backtrace-begin~%")
        (sb-debug:print-backtrace
         :stream stream
         :from :interrupted-frame
         :count 1000
         :print-thread t
         :emergency-best-effort t)
        (format stream "backtrace-end~%"))
    (error (backtrace-condition)
      (set-diagnostic-section stream "backtrace-error" backtrace-condition)))
  #-sbcl
  (format stream "backtrace-unavailable~%"))

(defun get-diagnostic-event (event &key session-name session-id message input condition)
  "Return one complete diagnostic record for EVENT."
  (with-output-to-string (stream)
    (format stream "record-begin~%")
    (format stream "timestamp=~A~%" (get-diagnostic-timestamp))
    (format stream "pid=~D~%" (get-diagnostic-process-id))
    (format stream "thread=~A~%" (get-diagnostic-thread-name))
    (format stream "event=~A~%" (string-downcase (string event)))
    (when session-id
      (format stream "session-id=~A~%" session-id))
    (when session-name
      (format stream "session-name=~A~%" session-name))
    (when message
      (set-diagnostic-section stream "message" message))
    (when input
      (set-diagnostic-section stream "input" input))
    (when condition
      (set-diagnostic-condition stream condition))
    (format stream "record-end~%")))

(defun get-pending-records (logger)
  "Wait for pending records, then return them in write order."
  (with-lock-held ((diagnostic-logger-lock logger))
    (loop while (and (null (diagnostic-logger-pending-records logger))
                     (not (diagnostic-logger-closed-p logger)))
          do (condition-wait (diagnostic-logger-condition logger)
                             (diagnostic-logger-lock logger)))
    (when (diagnostic-logger-pending-records logger)
      (prog1 (nreverse (diagnostic-logger-pending-records logger))
        (setf (diagnostic-logger-pending-records logger) nil)))))

(defun set-diagnostic-broadcaster (logger)
  "Broadcast queued records after each record reaches disk."
  (loop for records = (get-pending-records logger)
        while records
        do (dolist (record records)
             (ignore-errors
               (funcall (diagnostic-logger-broadcast-function logger)
                        record)))))

(defun new-diagnostic-logger (stream broadcast-function)
  "Create a diagnostic logger for STREAM and BROADCAST-FUNCTION."
  (check-type stream stream)
  (check-type broadcast-function function)
  (let ((logger (make-instance 'diagnostic-logger
                               :stream stream
                               :broadcast-function broadcast-function)))
    (setf (diagnostic-logger-worker-thread logger)
          (make-thread
           (lambda () (set-diagnostic-broadcaster logger))
           :name "lispore diagnostic broadcast"))
    logger))

(defun set-diagnostic-event (logger event &key session-name session-id message input condition)
  "Write EVENT to the diagnostic stream and queue its broadcast."
  (check-type logger diagnostic-logger)
  (let ((record (get-diagnostic-event
                 event
                 :session-name session-name
                 :session-id session-id
                 :message message
                 :input input
                 :condition condition)))
    (with-lock-held ((diagnostic-logger-lock logger))
      (unless (diagnostic-logger-closed-p logger)
        (write-string record (diagnostic-logger-stream logger))
        (finish-output (diagnostic-logger-stream logger))
        (push record (diagnostic-logger-pending-records logger))
        (condition-notify (diagnostic-logger-condition logger))
        t))))

(defun del-diagnostic-logger (logger)
  "Flush pending records, stop the broadcaster, and close LOGGER."
  (check-type logger diagnostic-logger)
  (let ((worker nil))
    (with-lock-held ((diagnostic-logger-lock logger))
      (setf (diagnostic-logger-closed-p logger) t
            worker (diagnostic-logger-worker-thread logger))
      (condition-notify (diagnostic-logger-condition logger)))
    (when (and worker
               (not (eq worker (current-thread))))
      (ignore-errors (join-thread worker)))
    (with-lock-held ((diagnostic-logger-lock logger))
      (unless (diagnostic-logger-stream-closed-p logger)
        (close (diagnostic-logger-stream logger))
        (setf (diagnostic-logger-stream-closed-p logger) t)))
    t))
