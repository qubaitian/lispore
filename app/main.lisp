(in-package #:lispore.app)

(defconstant +default-width+ 80)
(defconstant +default-height+ 24)
(defparameter *version* "0.1.0")

;; The application owns the manager and its cleanup lifecycle.
(defvar *session-manager* nil)

(defun positive-or-default (value default)
  "Return VALUE when it is positive, or DEFAULT otherwise."
  (if (and value (plusp value)) value default))

(defun terminal-dimensions ()
  "Return the current terminal size with safe fallback values."
  (multiple-value-bind (width height)
      (terminal-size 0)
    (values (positive-or-default width +default-width+)
            (positive-or-default height +default-height+))))

(defun ensure-session-manager ()
  "Return the application session manager, creating it when needed."
  (or *session-manager*
      (setf *session-manager* (make-session-manager))))

(defun run-new-session (manager)
  "Create and attach one command frontend through MANAGER."
  (multiple-value-bind (width height)
      (terminal-dimensions)
    (let* ((session-id (start-session manager
                                     :width width
                                     :height height))
           (attachment (attach-session manager session-id)))
      (unless attachment
        (error "The new shell session cannot accept an attachment."))
      (interactive-shell :attachment attachment))))

(defun application-handler (command)
  "Run one new managed shell session."
  (when (command-arguments command)
    (error "The lispore command does not accept arguments."))
  (let ((manager (ensure-session-manager)))
    (unwind-protect
         (run-new-session manager)
      (close-session-manager manager)
      (setf *session-manager* nil))))

(defun make-application-command ()
  "Return the root command for the Lispore application."
  (make-command :name "lispore"
                :description "Run an interactive shell."
                :usage "[options]"
                :version *version*
                :handler #'application-handler))

(defun main ()
  "Run the Lispore command-line application."
  (run (make-application-command)))
