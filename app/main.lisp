(in-package #:lispore.app)

(defparameter *version* "0.1.0")

(defun set-sbcl-debugger-disabled ()
  "Keep SBCL conditions inside Lispore's error reporting paths."
  #+sbcl
  (sb-ext:disable-debugger))

(defun set-unhandled-condition-report (condition)
  "Print CONDITION and its backtrace to the process error stream."
  (format *error-output* "Unhandled Lispore error: ~A~%" condition)
  #+sbcl
  (handler-case
      (sb-debug:print-backtrace
       :stream *error-output*
       :from :interrupted-frame
       :count 1000
       :print-thread t
       :emergency-best-effort t)
    (error (backtrace-condition)
      (format *error-output* "Backtrace error: ~A~%" backtrace-condition)))
  (finish-output *error-output*))

(defun set-application-usage-error (operation)
  "Signal the complete usage for an operation with two forms."
  (error "Usage:~%  lispore ~A session-manager~%  lispore ~A session <name>"
         operation
         operation))

(defun set-application-operation (command)
  "Apply one NEW, GET, SET, or DEL CLI operation."
  (let ((arguments (command-arguments command)))
    (unless arguments
      (error "The CLI requires NEW, GET, SET, or DEL."))
    (let ((operation (first arguments)))
      (cond
        ((string-equal operation "NEW")
         (cond
           ((and (= (length arguments) 2)
                 (string-equal (second arguments) "SESSION-MANAGER"))
            (new-cli-session-manager))
           ((and (= (length arguments) 3)
                 (string-equal (second arguments) "SESSION"))
            (new-cli-session (third arguments)))
           (t
            (set-application-usage-error "new"))))
        ((string-equal operation "GET")
         (unless (= (length arguments) 2)
           (error "Usage: lispore get <session-manager|session|debug|current-session>"))
         (let ((path (second arguments)))
           (cond
             ((string-equal path "SESSION-MANAGER")
              (get-cli-session-manager))
             ((string-equal path "SESSION")
              (get-cli-session-list))
             ((string-equal path "DEBUG")
              (get-cli-debug))
             ((string-equal path "CURRENT-SESSION")
              (error "The CLI current-session position is local to one process."))
             (t
              (error "GET does not support position ~A." path)))))
        ((string-equal operation "SET")
         (unless (and (>= (length arguments) 3)
                      (or (string-equal (second arguments) "DEBUG")
                          (string-equal (second arguments) "CURRENT-SESSION")))
           (error "Usage: lispore set <debug|current-session> <value>"))
         (let ((path (second arguments)))
           (cond
             ((string-equal path "DEBUG")
              (unless (= (length arguments) 3)
                (error "Usage: lispore set debug <0|1>"))
              (set-cli-debug (third arguments)))
             (t
              (unless (= (length arguments) 3)
                (error "Usage: lispore set current-session <name>"))
              (set-cli-current-session-frontend (third arguments))))))
        ((string-equal operation "DEL")
         (cond
           ((and (= (length arguments) 2)
                 (string-equal (second arguments) "SESSION-MANAGER"))
            (del-cli-session-manager))
           ((and (= (length arguments) 3)
                 (string-equal (second arguments) "SESSION"))
            (del-cli-session (third arguments)))
           (t
            (set-application-usage-error "del"))))
        (t
         (error "Unknown CLI operation ~A." operation))))))

(defun new-application-command ()
  "Return the root command for the Lispore application."
  (make-command :name "lispore"
                :description "Manage named shell sessions."
                :usage "<new|get|set|del> <path> [value]"
                :version *version*
                :handler #'set-application-operation))

(defun main ()
  "Start the Lispore command-line application."
  (set-sbcl-debugger-disabled)
  (handler-case
      (if (member +manager-server-argument+
                  (uiop:command-line-arguments)
                  :test #'string=)
          (set-manager-server)
          (run (new-application-command)))
    (error (condition)
      (set-unhandled-condition-report condition)
      (uiop:quit 1))))
