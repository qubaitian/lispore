(in-package #:lispore.app)

(defparameter *version* "0.1.0")

(defun disable-sbcl-debugger ()
  "Keep SBCL conditions inside Lispore's error reporting paths."
  #+sbcl
  (sb-ext:disable-debugger))

(defun report-unhandled-condition (condition)
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

(defun application-handler (command)
  "List sessions, or attach to one named session."
  (let ((arguments (command-arguments command)))
    (cond
      ((null arguments)
       (print-session-list))
      ((null (rest arguments))
       (run-named-session (first arguments)))
      (t
       (error "The lispore command accepts at most one session name.")))))

(defun debug-handler (command)
  "Enable manager diagnostics, then print the current session list."
  (when (command-arguments command)
    (error "The debug command accepts no arguments."))
  (let ((path (request-manager-debug)))
    (format t "Debug logging enabled: ~A~%" (namestring path))
    (print-session-list)))

(defun make-debug-command ()
  "Return the diagnostic logging subcommand."
  (make-command :name "debug"
                :description "Enable manager diagnostics."
                :handler #'debug-handler))

(defun make-application-command ()
  "Return the root command for the Lispore application."
  (make-command :name "lispore"
                :description "Manage named shell sessions."
                :usage "[options] [session-name]"
                :version *version*
                :handler #'application-handler
                :sub-commands (list (make-debug-command))))

(defun main ()
  "Run the Lispore command-line application."
  (disable-sbcl-debugger)
  (handler-case
      (if (member +manager-server-argument+
                  (uiop:command-line-arguments)
                  :test #'string=)
          (run-manager-server)
          (run (make-application-command)))
    (error (condition)
      (report-unhandled-condition condition)
      (uiop:quit 1))))
