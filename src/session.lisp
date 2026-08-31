(in-package #:lispore.session)

(defconstant +default-retention-seconds+ 300)
(defconstant +default-buffer-bytes+ (* 1024 1024))
(defconstant +session-read-size+ 4096)
(defconstant +session-cleanup-interval+ 0.1)
(defconstant +execution-interrupt-tag+ 'lispore.execution-interrupted)

(defclass session-manager ()
  ((sessions
    :initform (make-hash-table :test #'equal)
    :accessor manager-sessions)
   (named-sessions
    :initform (make-hash-table :test #'equal)
    :accessor manager-named-sessions)
   (lock
    :initform (make-lock "lispore session manager")
    :reader manager-lock)
   (next-id
    :initform 0
    :accessor manager-next-id)
   (retention-seconds
    :initarg :retention-seconds
    :reader manager-retention-seconds)
   (max-buffer-bytes
    :initarg :max-buffer-bytes
    :reader manager-max-buffer-bytes)
   (cleanup-thread
    :initform nil
    :accessor manager-cleanup-thread)
   (logger
    :initform nil
    :accessor manager-logger)
   (state
    :initform (list :debug 0)
    :accessor manager-state)
   (closed-p
    :initform nil
    :accessor manager-closed-p))
  (:documentation "Own managed shell sessions and their attachment registry."))

(defclass managed-session ()
  ((id
    :initarg :id
    :reader session-id)
   (name
    :initarg :name
    :initform nil
    :reader session-name)
   (manager
    :initarg :manager
    :reader session-manager)
   (shell-session
    :initarg :shell-session
    :reader managed-shell-session)
   (terminal
    :initarg :terminal
    :reader managed-terminal)
   (lock
    :initform (make-lock "lispore managed session")
    :reader session-lock)
   (input-lock
    :initform (make-lock "lispore session input")
    :reader session-input-lock)
   (read-lock
    :initform (make-lock "lispore session PTY read")
    :reader session-read-lock)
   (write-lock
    :initform (make-lock "lispore session PTY write")
    :reader session-write-lock)
   (attachments
    :initform nil
    :accessor session-attachments)
   (running-p
    :initform t
    :accessor managed-session-running-p)
   (terminated-p
    :initform nil
    :accessor managed-session-terminated-p)
   (retention-deadline
    :initform nil
    :accessor session-retention-deadline)
   (reader-thread
    :initform nil
    :accessor session-reader-thread)
   (error
    :initform nil
    :accessor managed-session-error)
   (pending-bytes
    :initform nil
    :accessor session-pending-bytes)
   (command-mode-p
    :initarg :command-mode-p
    :initform nil
    :reader command-mode-p)
   (input-history
    :initform nil
    :accessor session-input-history)
   (execution-state
    :initform :ready
    :accessor managed-execution-state)
   (execution-error
    :initform nil
    :accessor managed-execution-error)
   (execution-condition
    :initform (make-condition-variable :name "lispore command execution")
    :reader execution-condition)
   (execution-queue
    :initform nil
    :accessor execution-queue)
   (execution-thread
    :initform nil
    :accessor execution-thread)
   (execution-stop-p
    :initform nil
    :accessor execution-stop-p)
   (execution-marker
    :initform nil
    :accessor execution-marker)
   (execution-marker-buffer
    :initform ""
    :accessor execution-marker-buffer)
   (execution-kind
    :initform nil
    :accessor execution-kind)
   (execution-interrupted-p
    :initform nil
    :accessor execution-interrupted-p)
   (execution-job-started-p
    :initform nil
    :accessor execution-job-started-p)
   (execution-shell-input-written-p
    :initform nil
    :accessor execution-shell-input-written-p)
   (execution-attachment
    :initform nil
    :accessor execution-attachment)
   (execution-input
    :initform nil
    :accessor execution-input)
   (lisp-package
    :initarg :lisp-package
    :initform nil
    :accessor managed-lisp-package))
  (:documentation "Store one shell session and its shared terminal state."))

(defclass attachment ()
  ((session
    :initarg :session
    :reader attachment-session)
   (start-screen
    :initarg :start-screen
    :reader stored-attachment-start-screen)
   (final-screen
    :initform nil
    :accessor attachment-final-screen)
   (mode
    :initarg :mode
    :reader attachment-mode)
   (condition
    :initform (make-condition-variable :name "lispore attachment output")
    :reader attachment-condition)
   (buffer
    :initform nil
    :accessor attachment-buffer)
   (buffer-bytes
    :initform 0
    :accessor attachment-buffer-bytes)
   (max-buffer-bytes
    :initarg :max-buffer-bytes
    :reader attachment-max-buffer-bytes)
   (attached-p
    :initform t
    :accessor managed-attachment-attached-p)
   (input-editor
    :initarg :input-editor
    :reader attachment-input-editor))
  (:documentation "Connect one terminal frontend to a managed session."))

(defun new-session-manager (&key
                               (retention-seconds +default-retention-seconds+)
                               (max-buffer-bytes +default-buffer-bytes+))
  "Create an in-process registry for managed shell sessions."
  (check-type retention-seconds (real 0))
  (check-type max-buffer-bytes (integer 1))
  (let ((manager (make-instance 'session-manager
                                :retention-seconds retention-seconds
                                :max-buffer-bytes max-buffer-bytes)))
    (setf (manager-cleanup-thread manager)
          (make-thread
           (lambda () (set-session-cleaner manager))
           :name "lispore session cleanup"))
    manager))

(defun get-session-manager-state (manager)
  "Return MANAGER's lifecycle state."
  (check-type manager session-manager)
  (with-lock-held ((manager-lock manager))
    (if (manager-closed-p manager)
        :stopped
        :running)))

(defun set-session-manager-logger (manager logger)
  "Install LOGGER unless MANAGER already has one."
  (check-type manager session-manager)
  (check-type logger lispore.logging:diagnostic-logger)
  (with-lock-held ((manager-lock manager))
    (unless (manager-closed-p manager)
      (or (manager-logger manager)
          (progn
            (setf (manager-logger manager) logger
                  (getf (manager-state manager) :debug) 1)
            logger)))))

(defun del-session-manager-logger (manager)
  "Remove MANAGER's diagnostic logger."
  (check-type manager session-manager)
  (let ((logger nil))
    (with-lock-held ((manager-lock manager))
      (setf logger (manager-logger manager)
            (manager-logger manager) nil
            (getf (manager-state manager) :debug) 0))
    (when logger
      (del-diagnostic-logger logger))
    t))

(defun get-manager-debug-value (manager)
  "Return MANAGER's debug value."
  (check-type manager session-manager)
  (with-lock-held ((manager-lock manager))
    (getf (manager-state manager) :debug)))

(defun set-manager-debug-value (manager value)
  "Set MANAGER's debug value to zero or one."
  (check-type manager session-manager)
  (unless (member value '(0 1) :test #'eql)
    (error "Debug value must be 0 or 1."))
  (with-lock-held ((manager-lock manager))
    (when (manager-closed-p manager)
      (error "The shell session manager is closed."))
    (setf (getf (manager-state manager) :debug) value))
  value)

(defun get-manager-debug-enabled-p (manager)
  "Return true when MANAGER writes diagnostic records."
  (check-type manager session-manager)
  (plusp (get-manager-debug-value manager)))

(defun set-manager-log (manager event &key session-name session-id message input condition)
  "Write EVENT when MANAGER has diagnostic logging enabled."
  (check-type manager session-manager)
  (let ((logger (manager-logger manager)))
    (when logger
      (set-diagnostic-event logger
                            event
                            :session-name session-name
                            :session-id session-id
                            :message message
                            :input input
                            :condition condition))))

(defun set-session-log (session event &key message input condition)
  "Write EVENT with SESSION context without affecting session work."
  (ignore-errors
    (set-manager-log (session-manager session)
                 event
                 :session-name (session-name session)
                 :session-id (session-id session)
                 :message message
                 :input input
                 :condition condition)))

(defun get-current-shell ()
  "Return the shell selected by the environment."
  (or (uiop:getenv "SHELL") "/bin/sh"))

(defun shell-height (height)
  "Return the PTY rows below the frontend status line."
  (max 1 (1- height)))

(defun new-session-terminal (width height)
  "Create a session terminal with a reserved status row."
  (let ((terminal (new-terminal-emulator
                   :width width
                   :height height
                   :content-height (shell-height height))))
    (set-terminal-status-line terminal *default-status-line-text*)
    terminal))

(defun get-next-session-id (manager)
  "Return the next opaque identifier for MANAGER."
  (format nil "session-~D"
          (incf (manager-next-id manager))))

(defun get-next-lisp-package-name ()
  "Return a process-unique package name for one command session."
  (symbol-name (gensym "LISPORE-USER-")))

(defparameter +command-shell-ready-marker+ "__LISPORE_COMMAND_READY__")

(defun marker-count (marker text)
  "Count MARKER occurrences in TEXT."
  (loop with start = 0
        for position = (search marker text :start2 start)
        while position
        count 1
        do (setf start (1+ position))))

(defun set-command-shell (shell-session)
  "Hide the interactive shell prompt and input echo for the command frontend."
  (set-shell-input
   shell-session
   (format nil
           "stty -echo; PS1=''; PS2=''; export PS1 PS2; command printf '~A\\n'~%"
           +command-shell-ready-marker+))
  (loop with output = ""
        do (multiple-value-bind (bytes eof-p)
               (get-shell-output-bytes shell-session :wait-p t)
             (when eof-p
               (error "The command shell closed during initialization."))
             (when (and bytes (plusp (length bytes)))
               (setf output
                     (concatenate 'string
                                  output
                                  (map 'string #'code-char bytes)))
               (let ((position (search +command-shell-ready-marker+ output)))
                 (when (or (and position
                                (< (+ position
                                      (length +command-shell-ready-marker+))
                                   (length output))
                                (member
                                 (char output
                                       (+ position
                                          (length +command-shell-ready-marker+)))
                                 '(#\Return #\Newline)))
                            (>= (marker-count +command-shell-ready-marker+
                                              output)
                                2))
                   (return t)))))))

(defun get-session-running-under-lock-p (session)
  "Return true when SESSION accepts new work."
  (and (managed-session-running-p session)
       (not (managed-session-terminated-p session))))

(defun get-session-expired-p (session now)
  "Return true when SESSION's retained display has expired."
  (with-lock-held ((session-lock session))
    (and (managed-session-terminated-p session)
         (session-retention-deadline session)
         (>= now (session-retention-deadline session)))))

(defun get-session-screen-available-under-lock-p (session)
  "Return true while SESSION's display remains retained."
  (not (and (managed-session-terminated-p session)
            (session-retention-deadline session)
            (>= (get-internal-real-time)
                (session-retention-deadline session)))))

(defun del-expired-sessions (manager)
  "Remove terminated sessions past their retention deadline."
  (let ((now (get-internal-real-time))
        (expired-ids nil))
    (maphash (lambda (id session)
              (when (get-session-expired-p session now)
                (push id expired-ids)))
            (manager-sessions manager))
    (mapc (lambda (id)
            (let ((session (gethash id (manager-sessions manager))))
              (remhash id (manager-sessions manager))
              (when (and session (session-name session))
                (remhash (session-name session)
                         (manager-named-sessions manager)))))
          expired-ids)))

(defun set-session-cleaner (manager)
  "Remove expired sessions while MANAGER remains open."
  (loop
    (sleep +session-cleanup-interval+)
    (with-lock-held ((manager-lock manager))
      (when (manager-closed-p manager)
        (return))
      (del-expired-sessions manager))))

(defun del-managed-shell-session (session)
  "Close SESSION after its PTY reads and writes finish."
  (with-lock-held ((session-read-lock session))
    (with-lock-held ((session-write-lock session))
      (ignore-errors (del-shell-session (managed-shell-session session))))))

(defun get-session (manager session-id)
  "Return SESSION-ID's managed session while its retained record exists."
  (check-type manager session-manager)
  (with-lock-held ((manager-lock manager))
    (unless (manager-closed-p manager)
      (del-expired-sessions manager)
      (gethash session-id (manager-sessions manager)))))

(defun get-valid-session-mode-p (mode)
  "Return true when MODE names a supported frontend mode."
  (member mode '(:passthrough :command) :test #'eq))

(defun get-valid-session-name-p (name)
  "Return true when NAME is a simple session name."
  (and (stringp name)
       (plusp (length name))
       (every (lambda (character)
                (or (alphanumericp character)
                    (find character "._-" :test #'char=)))
              name)))

(defun get-checked-session-name (name)
  "Signal an error when NAME is not a valid session name."
  (unless (get-valid-session-name-p name)
    (error "Session names use letters, digits, dots, dashes, and underscores."))
  name)

(defun get-session-by-name (manager name)
  "Return the managed session named NAME while its record exists."
  (check-type manager session-manager)
  (get-checked-session-name name)
  (with-lock-held ((manager-lock manager))
    (unless (manager-closed-p manager)
      (del-expired-sessions manager)
      (gethash name (manager-named-sessions manager)))))

(defun get-session-list-state-under-lock (session)
  "Return SESSION's display state while SESSION is locked."
  (cond
    ((managed-session-terminated-p session) :closed)
    ((and (eq (managed-execution-state session) :ready)
          (managed-execution-error session))
     :error)
    (t
     (managed-execution-state session))))

(defun get-session-list (manager)
  "Return named sessions as NAME and display-state conses."
  (check-type manager session-manager)
  (with-lock-held ((manager-lock manager))
    (unless (manager-closed-p manager)
      (del-expired-sessions manager)
      (sort
       (loop for session being the hash-values of (manager-named-sessions manager)
             collect
             (with-lock-held ((session-lock session))
               (cons (session-name session)
                     (get-session-list-state-under-lock session))))
       #'string<
       :key #'car))))

(defun new-session (manager &key
                                name
                                (shell (get-current-shell))
                                (width 80)
                                (height 24)
                                (mode :command))
  "Start a fixed-size shell and return its opaque registry ID."
  (check-type manager session-manager)
  (when name
    (get-checked-session-name name))
  (check-type width (integer 1))
  (check-type height (integer 1))
  (unless (get-valid-session-mode-p mode)
    (error "Session mode must be :PASSTHROUGH or :COMMAND."))
  (let* ((shell-session (new-shell-session :shell shell
                                     :width width
                                     :height (shell-height height)))
         (managed-session nil)
         (session-id nil)
         (reader-started-p nil)
         (execution-started-p nil)
         (success-p nil))
    (unwind-protect
         (progn
           (when (eq mode :command)
             (set-command-shell shell-session))
           (with-lock-held ((manager-lock manager))
             (when (manager-closed-p manager)
               (error "The shell session manager is closed."))
             (del-expired-sessions manager)
             (when (and name
                        (gethash name (manager-named-sessions manager)))
               (error "A session named ~A already exists." name))
             (setf session-id (get-next-session-id manager)
                   managed-session
                   (make-instance 'managed-session
                                  :id session-id
                                  :name name
                                  :manager manager
                                  :shell-session shell-session
                                  :terminal (new-session-terminal width height)
                                  :command-mode-p (eq mode :command)
                                  :lisp-package
                                  (when (eq mode :command)
                                    (make-package
                                     (get-next-lisp-package-name)
                                     :use '(:cl))))
                   (gethash session-id (manager-sessions manager))
                   managed-session)
             (setf (session-reader-thread managed-session)
                   (make-thread
                    (lambda () (set-session-reader managed-session))
                    :name session-id)
                   reader-started-p t)
             (when (eq mode :command)
               (setf (execution-thread managed-session)
                     (make-thread
                      (lambda () (set-execution-worker managed-session))
                      :name (format nil "~A execution" session-id))
                     execution-started-p t))
             (when name
               (setf (gethash name (manager-named-sessions manager))
                     managed-session)))
           (setf success-p t)
           (set-session-log managed-session
                        "session-start"
                        :message (format nil "mode=~A shell=~A"
                                         mode
                                         shell))
           session-id)
      (unless success-p
        (when managed-session
          (with-lock-held ((manager-lock manager))
            (when (eq managed-session
                      (gethash session-id (manager-sessions manager)))
              (remhash session-id (manager-sessions manager))
              (when (and name
                         (eq managed-session
                             (gethash name (manager-named-sessions manager))))
                (remhash name (manager-named-sessions manager)))))
          (with-lock-held ((session-lock managed-session))
            (setf (managed-session-running-p managed-session) nil
                  (execution-stop-p managed-session) t)
            (condition-notify (execution-condition managed-session)))
          (del-managed-shell-session managed-session)
          (when (and reader-started-p
                     (not (eq (session-reader-thread managed-session)
                              (current-thread))))
            (ignore-errors
              (join-thread (session-reader-thread managed-session))))
          (when (and execution-started-p
                     (not (eq (execution-thread managed-session)
                              (current-thread))))
            (ignore-errors
              (join-thread (execution-thread managed-session))))
          (when (managed-lisp-package managed-session)
            (ignore-errors
              (delete-package (managed-lisp-package managed-session)))))
        (unless managed-session
          (del-shell-session shell-session))))))

(defun get-session-running-p (session)
  "Return true while SESSION accepts attachments and input."
  (check-type session managed-session)
  (with-lock-held ((session-lock session))
    (get-session-running-under-lock-p session)))

(defun get-session-error (session)
  "Return SESSION's reader error, if one occurred."
  (check-type session managed-session)
  (with-lock-held ((session-lock session))
    (managed-session-error session)))

(defun get-retained-screen (session)
  "Return a copy of SESSION's retained terminal screen."
  (check-type session managed-session)
  (with-lock-held ((session-lock session))
    (when (get-session-screen-available-under-lock-p session)
      (get-terminal-copy (managed-terminal session)))))

(defun get-attachment-start-screen (attachment)
  "Return the screen snapshot captured when ATTACHMENT started."
  (check-type attachment attachment)
  (let ((session (attachment-session attachment)))
    (with-lock-held ((session-lock session))
      (when (get-session-screen-available-under-lock-p session)
        (get-terminal-copy (stored-attachment-start-screen attachment))))))

(defun get-attachment-attached-p (attachment)
  "Return true while ATTACHMENT remains connected."
  (check-type attachment attachment)
  (let ((session (attachment-session attachment)))
    (with-lock-held ((session-lock session))
      (managed-attachment-attached-p attachment))))

(defun get-attachment-screen (attachment)
  "Return a copy of ATTACHMENT's current terminal screen."
  (check-type attachment attachment)
  (let ((session (attachment-session attachment)))
    (with-lock-held ((session-lock session))
      (when (get-session-screen-available-under-lock-p session)
        (if (attachment-final-screen attachment)
            (get-terminal-copy (attachment-final-screen attachment))
            (get-terminal-copy (managed-terminal session)))))))

(defun set-current-session (manager session-id &key (mode :command))
  "Attach a frontend to a running session and return its attachment."
  (check-type manager session-manager)
  (unless (get-valid-session-mode-p mode)
    (error "Attachment mode must be :PASSTHROUGH or :COMMAND."))
  (let ((session (get-session manager session-id)))
    (when session
      (multiple-value-bind (attachment screen)
          (with-lock-held ((session-lock session))
            (unless (eq (eq mode :command)
                        (command-mode-p session))
              (error "Attachment mode does not match the session mode."))
            (when (get-session-running-under-lock-p session)
              (let* ((start-screen (get-terminal-copy (managed-terminal session)))
                     (attachment
                       (make-instance 'attachment
                                      :session session
                                      :start-screen start-screen
                                      :mode mode
                                      :input-editor
                                      (new-input-editor
                                       :history (session-input-history session))
                                      :max-buffer-bytes
                                      (manager-max-buffer-bytes manager))))
                (push attachment (session-attachments session))
                (values attachment (get-terminal-copy start-screen)))))
        (when attachment
          (set-session-log session
                       "session-attach"
                       :message (format nil "mode=~A" mode)))
        (values attachment screen)))))

(defun del-attachment (attachment)
  "Remove ATTACHMENT from its session registry."
  (let ((session (attachment-session attachment)))
    (setf (session-attachments session)
          (delete attachment (session-attachments session) :test #'eq))))

(defun del-current-session (attachment)
  "Remove ATTACHMENT without closing its shell session."
  (check-type attachment attachment)
  (let ((session (attachment-session attachment)))
    (let ((detached-p nil))
      (with-lock-held ((session-lock session))
        (when (managed-attachment-attached-p attachment)
          (setf (managed-attachment-attached-p attachment) nil
                (attachment-buffer attachment) nil
                (attachment-buffer-bytes attachment) 0)
          (del-input-editor-draft-history (attachment-input-editor attachment))
          (del-attachment attachment)
          (condition-notify (attachment-condition attachment))
          (setf detached-p t)))
      (when detached-p
        (set-session-log session "session-detach"))
      detached-p)))

(defun set-attachment-output (attachment bytes)
  "Queue BYTES, or disconnect ATTACHMENT after overflow."
  (let ((new-size (+ (attachment-buffer-bytes attachment)
                     (length bytes))))
    (cond
      ((> new-size (attachment-max-buffer-bytes attachment))
       ;; A slow frontend leaves the registry after its buffer overflows.
       (del-input-editor-draft-history (attachment-input-editor attachment))
       (setf (managed-attachment-attached-p attachment) nil
             (attachment-buffer attachment) nil
             (attachment-buffer-bytes attachment) 0)
       (condition-notify (attachment-condition attachment))
       nil)
      (t
       (setf (attachment-buffer attachment)
             (nconc (attachment-buffer attachment)
                    (list (copy-seq bytes)))
             (attachment-buffer-bytes attachment) new-size)
       (condition-notify (attachment-condition attachment))
       t))))

(defun set-session-output-under-lock (session bytes)
  "Broadcast BYTES to every attachment while SESSION is locked."
  (when (and bytes (plusp (length bytes)))
    (dolist (attachment (copy-list (session-attachments session)))
      (unless (set-attachment-output attachment bytes)
        (del-attachment attachment)))))

(defun set-execution-finished-under-lock (session &optional condition)
  "Return SESSION to ready state after one command finishes."
  (setf (managed-execution-state session)
        (if (managed-session-terminated-p session) :closed :ready)
        (managed-execution-error session) condition
        (execution-marker session) nil
        (execution-marker-buffer session) ""
        (execution-kind session) nil
        (execution-interrupted-p session) nil
        (execution-job-started-p session) nil
        (execution-shell-input-written-p session) nil
        (execution-attachment session) nil
        (execution-input session) nil)
  (condition-notify (execution-condition session)))

(defun set-command-error-under-lock (attachment input condition)
  "Retain INPUT as ATTACHMENT's recovery draft after CONDITION."
  (when (managed-attachment-attached-p attachment)
    (let ((editor (attachment-input-editor attachment)))
      (set-input-editor-draft-history editor input)
      (when (zerop (length (get-input-editor-text editor)))
        (set-input-editor-draft editor input))))
  condition)

(defun get-shell-status-condition (status)
  "Return a condition for nonzero shell STATUS."
  (unless (zerop status)
    (make-condition 'simple-error
                    :format-control "Shell command exited with status ~D."
                    :format-arguments (list status))))

(defun set-current-execution-error-under-lock (session condition)
  "Retain the current command after a shell execution error."
  (when (and condition
             (execution-attachment session)
             (managed-attachment-attached-p (execution-attachment session)))
    (set-command-error-under-lock
     (execution-attachment session)
     (execution-input session)
     condition))
  condition)

(defun get-normalized-terminal-text (text)
  "Convert bare line feeds to CRLF in terminal-bound TEXT."
  ;; The terminal needs a carriage return before each line feed.
  (with-output-to-string (stream)
    (loop with after-return-p = nil
          for character across text
          do (cond
               ((char= character #\Return)
                (write-char character stream)
                (setf after-return-p t))
               ((char= character #\Newline)
                (unless after-return-p
                  (write-char #\Return stream))
                (write-char character stream)
                (setf after-return-p nil))
               (t
                (write-char character stream)
                (setf after-return-p nil))))))

(defun set-session-published-output (session text)
  "Publish Lispore TEXT as shared UTF-8 output."
  (check-type session managed-session)
  (check-type text string)
  (when (plusp (length text))
    (let* ((terminal-text (get-normalized-terminal-text text))
           (bytes (get-utf8 terminal-text)))
      (with-lock-held ((session-lock session))
        (when (get-session-running-under-lock-p session)
          (set-terminal-input (managed-terminal session) terminal-text)
          (set-session-output-under-lock session bytes)))))
  text)

(defun get-shell-marker (marker buffer)
  "Find the first marker with a valid or incomplete status line."
  (loop with start = 0
        for position = (search marker buffer :start2 start)
        while position
        do (let* ((status-start (+ position (length marker)))
                  (line-end
                   (position-if
                    (lambda (character)
                      (member character '(#\Return #\Newline)
                              :test #'char=))
                    buffer
                    :start status-start))
                  (status
                   (when (and line-end
                              (< status-start line-end)
                              (char= (char buffer status-start) #\:))
                     (handler-case
                         (parse-integer
                          buffer
                          :start (1+ status-start)
                          :end line-end)
                       (error () nil)))))
             (if (or (null line-end) (numberp status))
                 (return position)
                 (progn
                   ;; Ignore shell traces that echo marker text.
                   (setf start (1+ position)))))
        finally (return nil)))

(defun get-visible-shell-text-under-lock (session text)
  "Remove the worker marker from shell TEXT and finish its execution."
  (let ((marker (execution-marker session)))
    (if (null marker)
        (values text nil)
        (let* ((buffer (concatenate 'string
                                    (execution-marker-buffer session)
                                    text))
               (position (get-shell-marker marker buffer)))
          (cond
            ((null position)
             (let* ((keep (min (length marker) (length buffer)))
                    (visible-length (- (length buffer) keep)))
               ;; Keep a possible split marker for the next PTY chunk.
               (setf (execution-marker-buffer session)
                     (subseq buffer visible-length))
               (values (subseq buffer 0 visible-length) t)))
            (t
             (let* ((status-start (+ position (length marker)))
                    (line-end
                      (position-if
                       (lambda (character)
                         (member character '(#\Return #\Newline)
                                 :test #'char=))
                       buffer
                       :start status-start)))
               (if (null line-end)
                   (progn
                     ;; Keep partial marker data until the status line arrives.
                     (setf (execution-marker-buffer session)
                           (subseq buffer position))
                     (values (subseq buffer 0 position) t))
                   (let* ((status
                            (when (and (< status-start line-end)
                                       (char= (char buffer status-start) #\:))
                              (handler-case
                                  (parse-integer
                                   buffer
                                   :start (1+ status-start)
                                   :end line-end)
                                (error () nil))))
                          (condition
                            (if status
                                (get-shell-status-condition status)
                                (make-condition
                                 'simple-error
                                 :format-control
                                 "The shell execution marker is invalid."
                                 :format-arguments nil)))
                          (after-end (1+ line-end)))
                     ;; Parse status before releasing the execution worker.
                     (loop while (and (< after-end (length buffer))
                                      (member (char buffer after-end)
                                              '(#\Return #\Newline)
                                              :test #'char=))
                           do (incf after-end))
                     (set-current-execution-error-under-lock
                      session
                      condition)
                     (set-execution-finished-under-lock session condition)
                     ;; Remove only the marker and its status line.
                     (values
                      (concatenate 'string
                                   (subseq buffer 0 position)
                                   ;; Preserve output after the marker line.
                                   (subseq buffer after-end))
                      t))))))))))

(defun set-session-pty-output (session bytes)
  "Update SESSION's screen and broadcast BYTES to attachments."
  (with-lock-held ((session-lock session))
    (when (get-session-running-under-lock-p session)
      (multiple-value-bind (text pending)
          (get-utf8-chunk bytes (session-pending-bytes session))
        (setf (session-pending-bytes session) pending)
        (let ((marker-active-p (not (null (execution-marker session)))))
          (multiple-value-bind (visible-text filtered-p)
              (get-visible-shell-text-under-lock session text)
            (declare (ignore filtered-p))
            (when (plusp (length visible-text))
              (set-terminal-input (managed-terminal session) visible-text))
            (set-session-output-under-lock
             session
             (if marker-active-p
                 (get-utf8 visible-text)
                 bytes))))))))

(defun set-session-terminated (session &optional condition)
  "Mark SESSION terminated and wake its attached frontends."
  (let ((worker nil)
        (interrupt-worker-p nil))
    (with-lock-held ((session-lock session))
      (unless (managed-session-terminated-p session)
        (setf worker (execution-thread session)
              interrupt-worker-p
              (and worker
                   (not (eq worker (current-thread)))
                   (eq (managed-execution-state session) :running)
                   (eq (execution-kind session) :lisp)
                   (execution-job-started-p session)))
        (when interrupt-worker-p
          ;; Interrupt Lisp evaluation before closing this session.
          (ignore-errors
            (interrupt-thread
             worker
             (lambda ()
               (throw +execution-interrupt-tag+ :interrupted)))))
        (when (plusp (length (execution-marker-buffer session)))
          (let ((text (execution-marker-buffer session)))
            (set-terminal-input (managed-terminal session) text)
            (set-session-output-under-lock session (get-utf8 text))))
        (let ((execution-error
                (when (and (eq (managed-execution-state session) :running)
                           (eq (execution-kind session) :shell)
                           (execution-attachment session)
                           (managed-attachment-attached-p
                            (execution-attachment session)))
                  (or condition
                      (make-condition
                       'simple-error
                       :format-control
                       "The shell session ended during command execution."
                       :format-arguments nil)))))
          (when execution-error
            ;; Preserve a shell draft when the marker never arrives.
            (set-command-error-under-lock
             (execution-attachment session)
             (execution-input session)
             execution-error))
          (setf (managed-session-running-p session) nil
                (managed-session-terminated-p session) t
                (session-retention-deadline session)
                (+ (get-internal-real-time)
                   (round (* (manager-retention-seconds (session-manager session))
                              internal-time-units-per-second)))
                (managed-session-error session) condition
                (managed-execution-error session) execution-error
                (managed-execution-state session) :closed
                (execution-stop-p session) t
                (execution-marker session) nil
                (execution-marker-buffer session) ""
                (execution-kind session) nil
                (execution-interrupted-p session) t
                (execution-job-started-p session) nil
                (execution-shell-input-written-p session) nil
                (execution-attachment session) nil
                (execution-input session) nil)
          (condition-notify (execution-condition session))
          (let ((attachments (session-attachments session)))
            (setf (session-attachments session) nil)
            (dolist (attachment attachments)
              (setf (managed-attachment-attached-p attachment) nil
                    (attachment-final-screen attachment)
                    (get-terminal-copy (managed-terminal session)))
              (condition-notify (attachment-condition attachment)))))))
    (when (and worker
               (not (eq worker (current-thread))))
      (ignore-errors (join-thread worker)))
    t))

(defun set-session-reader (session)
  "Read PTY output in the background for SESSION."
  (let ((termination-condition nil))
    (handler-case
        (loop
          while (get-session-running-p session)
          do (multiple-value-bind (bytes eof-p)
               (with-lock-held ((session-read-lock session))
                 (get-shell-output-bytes (managed-shell-session session)
                                    :max-bytes +session-read-size+
                                    :wait-p nil))
               (when (and bytes (plusp (length bytes)))
                 (set-session-pty-output session bytes))
               (when eof-p
                 (return))
               (when (or (null bytes) (zerop (length bytes)))
                 (sleep 0.01))))
      (error (condition)
        (setf termination-condition condition)))
    (set-session-terminated session termination-condition)
    (set-session-log session
                 "session-terminated"
                 :message (if termination-condition "error" "eof")
                 :condition termination-condition)
    (del-managed-shell-session session)))

(defun get-attachment-output-chunk (attachment max-bytes)
  "Remove up to MAX-BYTES from ATTACHMENT's output buffer."
  (let* ((count (min max-bytes (attachment-buffer-bytes attachment)))
         (result (make-array count :element-type '(unsigned-byte 8)))
         (offset 0))
    (loop while (< offset count)
          for chunk = (first (attachment-buffer attachment))
          for amount = (min (- count offset) (length chunk))
          do (replace result chunk
                      :start1 offset
                      :end1 (+ offset amount)
                      :start2 0
                      :end2 amount)
             (incf offset amount)
             (decf (attachment-buffer-bytes attachment) amount)
             (if (= amount (length chunk))
                 (setf (attachment-buffer attachment)
                       (rest (attachment-buffer attachment)))
                 (setf (first (attachment-buffer attachment))
                       (subseq chunk amount))))
    result))

(defun get-attachment-output (attachment &key (max-bytes 4096) (wait-p t))
  "Read broadcast PTY bytes from ATTACHMENT."
  (check-type attachment attachment)
  (check-type max-bytes (integer 1))
  (let ((session (attachment-session attachment)))
    (with-lock-held ((session-lock session))
      (loop
        (cond
          ((plusp (attachment-buffer-bytes attachment))
           (return (values (get-attachment-output-chunk attachment max-bytes)
                           nil)))
          ((or (not (managed-attachment-attached-p attachment))
               (managed-session-terminated-p session))
           (return (values nil t)))
          ((not wait-p)
           (return (values #() nil)))
          (t
           (condition-wait (attachment-condition attachment)
                          (session-lock session))))))))

(defun get-input-history (object)
  "Return a copy of the session's newest-first input history."
  (let ((session (etypecase object
                   (managed-session object)
                   (attachment (attachment-session object)))))
    (with-lock-held ((session-lock session))
      (mapcar #'copy-seq (session-input-history session)))))

(defun get-execution-state (object)
  "Return the current execution state for a session or attachment."
  (let ((session (etypecase object
                   (managed-session object)
                   (attachment (attachment-session object)))))
    (with-lock-held ((session-lock session))
      (if (managed-session-terminated-p session)
          :closed
          (managed-execution-state session)))))

(defun get-input-draft (attachment)
  "Return ATTACHMENT's private input draft."
  (check-type attachment attachment)
  (let ((session (attachment-session attachment)))
    (with-lock-held ((session-lock session))
      (get-input-editor-text (attachment-input-editor attachment)))))

(defun get-input-cursor (attachment)
  "Return ATTACHMENT's input cursor position."
  (check-type attachment attachment)
  (let ((session (attachment-session attachment)))
    (with-lock-held ((session-lock session))
      (get-input-editor-cursor (attachment-input-editor attachment)))))

(defun set-input-draft (attachment text)
  "Set ATTACHMENT's private input draft."
  (check-type attachment attachment)
  (check-type text string)
  (let ((session (attachment-session attachment)))
    (with-lock-held ((session-lock session))
      (set-input-editor-draft (attachment-input-editor attachment) text)))
  attachment)

(defun get-valid-input-p (input)
  "Return true when INPUT contains valid transport bytes."
  (or (stringp input)
      (and (vectorp input)
           (every (lambda (byte)
                   (and (integerp byte) (<= 0 byte 255)))
                 input))))

(defun get-input-copy (input)
  "Return an independent copy of string or octet INPUT."
  (copy-seq input))

(defun set-input-submission (attachment &optional (input nil input-supplied-p))
  "Submit ATTACHMENT's draft, or explicit INPUT, without interleaving."
  (check-type attachment attachment)
  (when (and input-supplied-p (not (get-valid-input-p input)))
    (error "Input must be UTF-8 text or an octet vector."))
  (let ((session (attachment-session attachment)))
    (let ((explicit-input (when input-supplied-p
                            (get-input-copy input))))
      (when input-supplied-p
        (with-lock-held ((session-lock session))
          (when (and (not (command-mode-p session))
                     (stringp explicit-input))
            (set-input-editor-draft
             (attachment-input-editor attachment)
             explicit-input))))
      (when (acquire-lock (session-input-lock session) nil)
        (unwind-protect
             (multiple-value-bind (accepted-p value)
                 (with-lock-held ((session-lock session))
                   (if (not (and (not (command-mode-p session))
                                 (managed-attachment-attached-p attachment)
                                 (get-session-running-under-lock-p session)))
                       (values nil nil)
                       (values t
                               (if input-supplied-p
                                   explicit-input
                                   (get-input-copy
                                    (get-input-editor-text
                                     (attachment-input-editor attachment)))))))
               (when accepted-p
                 (set-session-log session "input-submit" :input value)
                 (handler-case
                     (progn
                       (with-lock-held ((session-write-lock session))
                         (set-shell-input
                          (managed-shell-session session)
                          value))
                       (with-lock-held ((session-lock session))
                         (del-input-editor
                          (attachment-input-editor attachment)))
                       t)
                   (error (condition)
                     (set-session-log session
                                  "input-error"
                                  :input value
                                  :condition condition)
                     nil))))
          (release-lock (session-input-lock session)))))))

(defun get-valid-command-kind-p (kind)
  "Return true when KIND names a command frontend language."
  (member kind '(:lisp :shell) :test #'eq))

(defun set-command-submission (attachment input kind)
  "Queue one complete command for ATTACHMENT's command frontend."
  (check-type attachment attachment)
  (check-type input string)
  (unless (get-valid-command-kind-p kind)
    (error "Command kind must be :LISP or :SHELL."))
  (unless (eq kind (get-input-language input))
    (return-from set-command-submission nil))
  (unless (member (get-input-completeness input) '(:complete :error) :test #'eq)
    (return-from set-command-submission nil))
  (let ((session (attachment-session attachment))
        (accepted-p nil))
    (with-lock-held ((session-lock session))
      (let* ((editor (attachment-input-editor attachment))
             (draft (get-input-editor-text editor)))
        (when (and (command-mode-p session)
                   (managed-attachment-attached-p attachment)
                   (get-session-running-under-lock-p session)
                   (eq (managed-execution-state session) :ready)
                   (plusp (length input))
                   (= (get-input-editor-cursor editor) (length draft))
                   (string= input draft))
          (setf (session-input-history session)
                (cons (copy-seq input) (session-input-history session))
                (managed-execution-state session) :running
                (managed-execution-error session) nil
                (execution-kind session) kind
                (execution-interrupted-p session) nil
                (execution-job-started-p session) nil
                (execution-shell-input-written-p session) nil
                (execution-attachment session) attachment
                (execution-input session) (copy-seq input)
                (execution-queue session)
                (nconc (execution-queue session)
                       (list (list attachment (copy-seq input) kind))))
          (set-input-editor-submission editor input)
          ;; Clear the accepted draft before the worker can report an error.
          (del-input-editor editor)
          (condition-notify (execution-condition session))
          (setf accepted-p t))))
    (when accepted-p
      (set-session-log session
                   "command-submit"
                   :message (format nil "kind=~A" kind)
                   :input input))
    accepted-p))

(defun set-execution-interruption (object)
  "Interrupt the active command for OBJECT, if one exists."
  (let* ((attachment (when (typep object 'attachment) object))
         (session (etypecase object
                    (managed-session object)
                    (attachment (attachment-session attachment))))
         (thread nil)
         (kind nil)
         (active-p nil)
         (job-started-p nil)
         (shell-input-written-p nil))
    (with-lock-held ((session-lock session))
      (setf active-p (eq (managed-execution-state session) :running)
            thread (execution-thread session)
            kind (execution-kind session)
            job-started-p (execution-job-started-p session)
            shell-input-written-p (execution-shell-input-written-p session)
            (execution-interrupted-p session) active-p))
    (when active-p
      (if (eq kind :shell)
          (when (and job-started-p shell-input-written-p)
            (with-lock-held ((session-write-lock session))
              (ignore-errors
                (set-shell-input (managed-shell-session session) (vector 3)))))
          (when (and job-started-p
                     thread
                     (not (eq thread (current-thread))))
            (ignore-errors
              (interrupt-thread
               thread
               (lambda ()
                 (throw +execution-interrupt-tag+ :interrupted))))))
      (set-session-log session
                   "execution-interrupt"
                   :message (format nil "kind=~A" kind))
      t)))

(defun get-next-execution-marker (session)
  "Return an opaque shell marker for SESSION."
  (declare (ignore session))
  (format nil "__LISPORE_COMMAND_DONE_~36R__"
          (random most-positive-fixnum)))

(defun set-shell-command (session attachment input)
  "Run INPUT through SESSION's persistent shell."
  (let ((token (get-next-execution-marker session))
        (command-condition nil))
    (with-lock-held ((session-lock session))
      (when (get-session-running-under-lock-p session)
        (setf (execution-marker session) token
              (execution-marker-buffer session) ""
              (execution-shell-input-written-p session) nil
              (execution-attachment session) attachment
              (execution-input session) (copy-seq input))))
    (handler-case
        (progn
          (with-lock-held ((session-write-lock session))
            (set-shell-input
             (managed-shell-session session)
             ;; The wrapper prints a private marker with the command status.
             (format nil "~A~%command printf '~A:%d\\n' $?~%"
                     input
                     token))
            ;; Send Ctrl-C after the command reaches the PTY.
            (with-lock-held ((session-lock session))
              (when (and (get-session-running-under-lock-p session)
                         (string= token (execution-marker session)))
                (setf (execution-shell-input-written-p session) t)
                (when (execution-interrupted-p session)
                  (ignore-errors
                    (set-shell-input
                     (managed-shell-session session)
                     (vector 3))))))
          (with-lock-held ((session-lock session))
            (loop while (and (get-session-running-under-lock-p session)
                             (eq (managed-execution-state session) :running))
                  do (condition-wait (execution-condition session)
                                     (session-lock session)))
            (setf command-condition (managed-execution-error session)))
          (when command-condition
            (set-session-log session
                         "command-error"
                         :input input
                         :condition command-condition))))
      (error (condition)
        (with-lock-held ((session-lock session))
          (set-command-error-under-lock
           attachment
           input
           condition)
          (set-execution-finished-under-lock session condition))
        (setf command-condition condition)
        (set-session-log session
                     "command-error"
                     :input input
                     :condition command-condition)))))

(defun get-lisp-evaluation (session input)
  "Evaluate INPUT in SESSION's persistent user package."
  (with-output-to-string (stream)
    (let ((*package* (managed-lisp-package session))
          (*read-eval* nil)
          (*standard-output* stream)
          (*error-output* stream)
          (*trace-output* stream))
      (let ((eof-marker (gensym "LISP-INPUT-EOF")))
        (with-input-from-string (input-stream input)
          (let ((form (read input-stream nil eof-marker)))
            (when (eq form eof-marker)
              (error "Lisp input is empty."))
            (let ((trailing-form (read input-stream nil eof-marker)))
              (unless (eq trailing-form eof-marker)
                (error "Lisp input contains trailing text.")))
            (format stream "~{~S~^ ~}~%"
                    (multiple-value-list (eval form)))))))))

(defun set-lisp-command (session attachment input)
  "Evaluate INPUT and publish its output."
  (let ((result
          (catch +execution-interrupt-tag+
            (handler-case
                (progn
                  (let ((output (get-lisp-evaluation session input)))
                    (set-session-published-output session output))
                  :finished)
              (error (caught-condition)
                (with-lock-held ((session-lock session))
                  (set-command-error-under-lock
                   attachment
                   input
                   caught-condition))
                (set-session-log session
                             "command-error"
                             :input input
                             :condition caught-condition)
                (set-session-published-output
                 session
                 (format nil "Error: ~A~%" caught-condition))
                (list :error caught-condition))))))
    (when (eq result :interrupted)
      (set-session-published-output
       session
       (format nil "Execution interrupted.~%")))
    (with-lock-held ((session-lock session))
      (set-execution-finished-under-lock
       session
       (when (and (consp result)
                  (eq (first result) :error))
         (second result))))))

(defun set-execution-job (session job)
  "Run one queued command JOB."
  (destructuring-bind (attachment input kind) job
    (let ((result
            (catch +execution-interrupt-tag+
              (let ((job-ready-p
                      (with-lock-held ((session-lock session))
                        (if (execution-interrupted-p session)
                            nil
                            (progn
                              (setf (execution-job-started-p session) t)
                              t)))))
                (if job-ready-p
                    (progn
                      (set-session-log session
                                   "command-start"
                                   :message (format nil "kind=~A" kind)
                                   :input input)
                      (ecase kind
                        (:lisp (set-lisp-command session attachment input))
                        (:shell (set-shell-command session attachment input)))
                      (set-session-log session
                                   "command-finish"
                                   :message (format nil "kind=~A" kind)
                                   :input input))
                    (progn
                      (set-session-log session
                                   "command-interrupted"
                                   :message (format nil "kind=~A" kind)
                                   :input input)
                      (set-session-published-output
                       session
                       (format nil "Execution interrupted.~%"))
                      (with-lock-held ((session-lock session))
                        (set-execution-finished-under-lock session)))))
              :finished)))
      (when (eq result :interrupted)
        (set-session-published-output
         session
         (format nil "Execution interrupted.~%"))
        (with-lock-held ((session-lock session))
          (set-execution-finished-under-lock session))))))

(defun set-execution-worker (session)
  "Serve SESSION's serialized command queue."
  (loop
    do (let ((job
               (with-lock-held ((session-lock session))
                 (loop
                   (when (execution-stop-p session)
                     (return-from set-execution-worker nil))
                   (when (execution-queue session)
                     (return (pop (execution-queue session))))
                   (condition-wait (execution-condition session)
                                  (session-lock session))))))
         (handler-case
             (set-execution-job session job)
           (error (condition)
             (with-lock-held ((session-lock session))
               (set-command-error-under-lock
                (first job)
                (second job)
                condition)
               (set-execution-finished-under-lock session condition))
             (set-session-log session
                          "execution-worker-error"
                          :input (second job)
                          :condition condition))))))

(defun del-managed-session (session)
  "Terminate SESSION and wait for its reader thread."
  (let ((thread nil)
        (worker nil))
    (with-lock-held ((session-lock session))
      (setf (managed-session-running-p session) nil
            (execution-stop-p session) t
            thread (session-reader-thread session)
            worker (execution-thread session))
      (condition-notify (execution-condition session)))
    ;; Stop an evaluator before waiting for the worker thread.
    (ignore-errors (set-execution-interruption session))
    (del-managed-shell-session session)
    (when (and thread
               (not (eq thread (current-thread))))
      (ignore-errors (join-thread thread)))
    (set-session-terminated session)
    (when (and worker
               (not (eq worker (current-thread))))
      (ignore-errors (join-thread worker)))
    (when (managed-lisp-package session)
      (ignore-errors (delete-package (managed-lisp-package session)))
      (setf (managed-lisp-package session) nil))
    t))

(defun del-session (manager session-id)
  "Terminate the session registered under SESSION-ID."
  (let ((session (get-session manager session-id)))
    (when session
      (del-managed-session session)
      (with-lock-held ((manager-lock manager))
        (when (eq session (gethash session-id (manager-sessions manager)))
          (remhash session-id (manager-sessions manager))
          (when (and (session-name session)
                     (eq session
                         (gethash (session-name session)
                                  (manager-named-sessions manager))))
            (remhash (session-name session)
                     (manager-named-sessions manager)))))
      t)))

(defun del-session-manager (manager)
  "Terminate every session and stop the in-process registry."
  (check-type manager session-manager)
  (set-manager-log manager "manager-close")
  (let ((sessions nil)
        (cleanup-thread nil)
        (logger nil))
    (with-lock-held ((manager-lock manager))
      (setf (manager-closed-p manager) t)
      (setf cleanup-thread (manager-cleanup-thread manager)
            logger (manager-logger manager))
      (maphash (lambda (id session)
                 (declare (ignore id))
                 (push session sessions))
               (manager-sessions manager)))
    (mapc #'del-managed-session sessions)
    (when (and cleanup-thread
               (not (eq cleanup-thread (current-thread))))
      (ignore-errors (join-thread cleanup-thread)))
    (with-lock-held ((manager-lock manager))
      (clrhash (manager-sessions manager))
      (clrhash (manager-named-sessions manager))
      (when (eq logger (manager-logger manager))
        (setf (manager-logger manager) nil)))
    (when logger
      (del-diagnostic-logger logger))
    t))
