(in-package #:lispore.app)

(defparameter *version* "0.1.0")

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

(defun make-application-command ()
  "Return the root command for the Lispore application."
  (make-command :name "lispore"
                :description "Manage named shell sessions."
                :usage "[options] [session-name]"
                :version *version*
                :handler #'application-handler))

(defun main ()
  "Run the Lispore command-line application."
  (if (member +manager-server-argument+
              (uiop:command-line-arguments)
              :test #'string=)
      (run-manager-server)
      (run (make-application-command))))
