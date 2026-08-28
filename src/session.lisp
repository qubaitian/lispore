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
   (closed-p
    :initform nil
    :accessor manager-closed-p))
  (:documentation "Own managed shell sessions and their attachment registry."))

(defclass managed-session ()
  ((id
    :initarg :id
    :reader session-id)
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
   (start-pending-bytes
    :initarg :start-pending-bytes
    :reader stored-attachment-start-pending-bytes)
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

(defun make-session-manager (&key
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
           (lambda () (run-session-cleaner manager))
           :name "lispore session cleanup"))
    manager))

(defun current-shell ()
  "Return the shell selected by the environment."
  (or (uiop:getenv "SHELL") "/bin/sh"))

(defun shell-height (height)
  "Return the PTY rows below the frontend status line."
  (max 1 (1- height)))

(defun make-session-terminal (width height)
  "Create a session terminal with a reserved status row."
  (let ((terminal (make-terminal-emulator
                   :width width
                   :height height
                   :content-height (shell-height height))))
    (set-status-line terminal *default-status-line-text*)
    terminal))

(defun next-session-id (manager)
  "Return the next opaque identifier for MANAGER."
  (format nil "session-~D"
          (incf (manager-next-id manager))))

(defun next-lisp-package-name ()
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

(defun prepare-command-shell (shell-session)
  "Hide the interactive shell prompt and input echo for command mode."
  (write-input
   shell-session
   (format nil
           "stty -echo; PS1=''; PS2=''; export PS1 PS2; command printf '~A\\n'~%"
           +command-shell-ready-marker+))
  (loop with output = ""
        do (multiple-value-bind (bytes eof-p)
               (read-output-bytes shell-session :wait-p t)
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

(defun session-running-under-lock-p (session)
  "Return true when SESSION accepts new work."
  (and (managed-session-running-p session)
       (not (managed-session-terminated-p session))))

(defun session-expired-p (session now)
  "Return true when SESSION's retained display has expired."
  (with-lock-held ((session-lock session))
    (and (managed-session-terminated-p session)
         (session-retention-deadline session)
         (>= now (session-retention-deadline session)))))

(defun session-screen-available-under-lock-p (session)
  "Return true while SESSION's display remains retained."
  (not (and (managed-session-terminated-p session)
            (session-retention-deadline session)
            (>= (get-internal-real-time)
                (session-retention-deadline session)))))

(defun purge-expired-sessions (manager)
  "Remove terminated sessions past their retention deadline."
  (let ((now (get-internal-real-time))
        (expired-ids nil))
    (maphash (lambda (id session)
              (when (session-expired-p session now)
                (push id expired-ids)))
            (manager-sessions manager))
    (mapc (lambda (id)
            (remhash id (manager-sessions manager)))
          expired-ids)))

(defun run-session-cleaner (manager)
  "Remove expired sessions while MANAGER remains open."
  (loop
    (sleep +session-cleanup-interval+)
    (with-lock-held ((manager-lock manager))
      (when (manager-closed-p manager)
        (return))
      (purge-expired-sessions manager))))

(defun close-managed-shell-session (session)
  "Close SESSION after its PTY reads and writes finish."
  (with-lock-held ((session-read-lock session))
    (with-lock-held ((session-write-lock session))
      (ignore-errors (close-session (managed-shell-session session))))))

(defun lookup-session (manager session-id)
  "Return SESSION-ID's managed session while its retained record exists."
  (check-type manager session-manager)
  (with-lock-held ((manager-lock manager))
    (unless (manager-closed-p manager)
      (purge-expired-sessions manager)
      (gethash session-id (manager-sessions manager)))))

(defun start-session (manager &key
                                (shell (current-shell))
                                (width 80)
                                (height 24)
                                (mode :emulated))
  "Start a fixed-size shell and return its opaque registry ID."
  (check-type manager session-manager)
  (check-type width (integer 1))
  (check-type height (integer 1))
  (unless (member mode '(:passthrough :emulated :command) :test #'eq)
    (error "Session mode must be :PASSTHROUGH, :EMULATED, or :COMMAND."))
  (let* ((shell-session (start-shell :shell shell
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
             (prepare-command-shell shell-session))
           (with-lock-held ((manager-lock manager))
             (when (manager-closed-p manager)
               (error "The shell session manager is closed."))
             (setf session-id (next-session-id manager)
                   managed-session
                   (make-instance 'managed-session
                                  :id session-id
                                  :manager manager
                                  :shell-session shell-session
                                  :terminal (make-session-terminal width height)
                                  :command-mode-p (eq mode :command)
                                  :lisp-package
                                  (when (eq mode :command)
                                    (make-package
                                     (next-lisp-package-name)
                                     :use '(:cl))))
                   (gethash session-id (manager-sessions manager))
                   managed-session)
             (setf (session-reader-thread managed-session)
                   (make-thread
                    (lambda () (run-session-reader managed-session))
                    :name session-id)
                   reader-started-p t)
             (when (eq mode :command)
               (setf (execution-thread managed-session)
                     (make-thread
                      (lambda () (run-execution-worker managed-session))
                      :name (format nil "~A execution" session-id))
                     execution-started-p t)))
           (setf success-p t)
           session-id)
      (unless success-p
        (when managed-session
          (with-lock-held ((manager-lock manager))
            (when (eq managed-session
                      (gethash session-id (manager-sessions manager)))
              (remhash session-id (manager-sessions manager))))
          (with-lock-held ((session-lock managed-session))
            (setf (managed-session-running-p managed-session) nil
                  (execution-stop-p managed-session) t)
            (condition-notify (execution-condition managed-session)))
          (close-managed-shell-session managed-session)
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
          (close-session shell-session))))))

(defun session-running-p (session)
  "Return true while SESSION accepts attachments and input."
  (check-type session managed-session)
  (with-lock-held ((session-lock session))
    (session-running-under-lock-p session)))

(defun session-error (session)
  "Return SESSION's reader error, if one occurred."
  (check-type session managed-session)
  (with-lock-held ((session-lock session))
    (managed-session-error session)))

(defun retained-screen (session)
  "Return a copy of SESSION's retained terminal screen."
  (check-type session managed-session)
  (with-lock-held ((session-lock session))
    (when (session-screen-available-under-lock-p session)
      (copy-terminal (managed-terminal session)))))

(defun attachment-start-screen (attachment)
  "Return the screen snapshot captured when ATTACHMENT started."
  (check-type attachment attachment)
  (let ((session (attachment-session attachment)))
    (with-lock-held ((session-lock session))
      (when (session-screen-available-under-lock-p session)
        (values (copy-terminal (stored-attachment-start-screen attachment))
                (and (stored-attachment-start-pending-bytes attachment)
                     (copy-seq
                      (stored-attachment-start-pending-bytes attachment))))))))

(defun attachment-attached-p (attachment)
  "Return true while ATTACHMENT remains connected."
  (check-type attachment attachment)
  (let ((session (attachment-session attachment)))
    (with-lock-held ((session-lock session))
      (managed-attachment-attached-p attachment))))

(defun attachment-screen (attachment)
  "Return a copy of ATTACHMENT's current terminal screen."
  (check-type attachment attachment)
  (let ((session (attachment-session attachment)))
    (with-lock-held ((session-lock session))
      (when (session-screen-available-under-lock-p session)
        (if (attachment-final-screen attachment)
            (copy-terminal (attachment-final-screen attachment))
            (copy-terminal (managed-terminal session)))))))

(defun valid-attachment-mode-p (mode)
  "Return true when MODE names a supported frontend mode."
  (member mode '(:passthrough :emulated :command) :test #'eq))

(defun attach-session (manager session-id &key (mode :emulated))
  "Attach a frontend to a running session and return its attachment."
  (check-type manager session-manager)
  (unless (valid-attachment-mode-p mode)
    (error "Attachment mode must be :PASSTHROUGH, :EMULATED, or :COMMAND."))
  (let ((session (lookup-session manager session-id)))
    (when session
      (with-lock-held ((session-lock session))
        (unless (eq (eq mode :command)
                    (command-mode-p session))
          (error "Attachment mode does not match the session mode."))
        (when (session-running-under-lock-p session)
          (let* ((start-screen (copy-terminal (managed-terminal session)))
                 (attachment
                   (make-instance 'attachment
                                  :session session
                                  :start-screen start-screen
                                  :start-pending-bytes
                                  (and (session-pending-bytes session)
                                       (copy-seq
                                        (session-pending-bytes session)))
                                  :mode mode
                                  :input-editor
                                  (make-input-editor
                                   :history (session-input-history session))
                                  :max-buffer-bytes
                                  (manager-max-buffer-bytes manager))))
            (push attachment (session-attachments session))
            (values attachment (copy-terminal start-screen))))))))

(defun restore-session (manager session-id &key (mode :emulated))
  "Reattach to a running session and return its retained screen."
  (multiple-value-bind (attachment screen)
      (attach-session manager session-id :mode mode)
    (when attachment
      (values attachment screen))))

(defun reattach-session (manager session-id &key (mode :emulated))
  "Reattach to a running session using the canonical lifecycle term."
  (restore-session manager session-id :mode mode))

(defun remove-attachment (attachment)
  "Remove ATTACHMENT from its session registry."
  (let ((session (attachment-session attachment)))
    (setf (session-attachments session)
          (delete attachment (session-attachments session) :test #'eq))))

(defun detach (attachment)
  "Remove ATTACHMENT without closing its shell session."
  (check-type attachment attachment)
  (let ((session (attachment-session attachment)))
    (with-lock-held ((session-lock session))
      (when (managed-attachment-attached-p attachment)
        (setf (managed-attachment-attached-p attachment) nil
              (attachment-buffer attachment) nil
              (attachment-buffer-bytes attachment) 0)
        (input-editor-clear-draft-history (attachment-input-editor attachment))
        (remove-attachment attachment)
        (condition-notify (attachment-condition attachment))
        t))))

(defun append-attachment-output (attachment bytes)
  "Queue BYTES, or disconnect ATTACHMENT after overflow."
  (let ((new-size (+ (attachment-buffer-bytes attachment)
                     (length bytes))))
    (cond
      ((> new-size (attachment-max-buffer-bytes attachment))
       ;; A slow frontend leaves the registry after its buffer overflows.
       (input-editor-clear-draft-history (attachment-input-editor attachment))
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

(defun broadcast-session-bytes-under-lock (session bytes)
  "Broadcast BYTES to every attachment while SESSION is locked."
  (when (and bytes (plusp (length bytes)))
    (dolist (attachment (copy-list (session-attachments session)))
      (unless (append-attachment-output attachment bytes)
        (remove-attachment attachment)))))

(defun finish-execution-under-lock (session &optional condition)
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

(defun record-command-error-under-lock (attachment input condition)
  "Retain INPUT as ATTACHMENT's recovery draft after CONDITION."
  (when (managed-attachment-attached-p attachment)
    (let ((editor (attachment-input-editor attachment)))
      (input-editor-add-draft-history editor input)
      (when (zerop (length (input-editor-text editor)))
        (input-editor-set-draft editor input))))
  condition)

(defun shell-status-condition (status)
  "Return a condition for nonzero shell STATUS."
  (unless (zerop status)
    (make-condition 'simple-error
                    :format-control "Shell command exited with status ~D."
                    :format-arguments (list status))))

(defun record-current-execution-error-under-lock (session condition)
  "Retain the current command after a shell execution error."
  (when (and condition
             (execution-attachment session)
             (managed-attachment-attached-p (execution-attachment session)))
    (record-command-error-under-lock
     (execution-attachment session)
     (execution-input session)
     condition))
  condition)

(defun publish-session-output (session text)
  "Publish Lispore TEXT as shared UTF-8 output."
  (check-type session managed-session)
  (check-type text string)
  (when (plusp (length text))
    (let ((bytes (encode-utf8 text)))
      (with-lock-held ((session-lock session))
        (when (session-running-under-lock-p session)
          (feed-terminal (managed-terminal session) text)
          (broadcast-session-bytes-under-lock session bytes)))))
  text)

(defun find-shell-marker (marker buffer)
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

(defun visible-shell-text-under-lock (session text)
  "Remove the worker marker from shell TEXT and finish its execution."
  (let ((marker (execution-marker session)))
    (if (null marker)
        (values text nil)
        (let* ((buffer (concatenate 'string
                                    (execution-marker-buffer session)
                                    text))
               (position (find-shell-marker marker buffer)))
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
                                (shell-status-condition status)
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
                     (record-current-execution-error-under-lock
                      session
                      condition)
                     (finish-execution-under-lock session condition)
                     ;; Remove only the marker and its status line.
                     (values
                      (concatenate 'string
                                   (subseq buffer 0 position)
                                   ;; Preserve output after the marker line.
                                   (subseq buffer after-end))
                      t))))))))))

(defun record-session-output (session bytes)
  "Update SESSION's screen and broadcast BYTES to attachments."
  (with-lock-held ((session-lock session))
    (when (session-running-under-lock-p session)
      (multiple-value-bind (text pending)
          (decode-utf8-chunk bytes (session-pending-bytes session))
        (setf (session-pending-bytes session) pending)
        (let ((marker-active-p (not (null (execution-marker session)))))
          (multiple-value-bind (visible-text filtered-p)
              (visible-shell-text-under-lock session text)
            (declare (ignore filtered-p))
            (when (plusp (length visible-text))
              (feed-terminal (managed-terminal session) visible-text))
            (broadcast-session-bytes-under-lock
             session
             (if marker-active-p
                 (encode-utf8 visible-text)
                 bytes))))))))

(defun mark-session-terminated (session &optional condition)
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
            (feed-terminal (managed-terminal session) text)
            (broadcast-session-bytes-under-lock session (encode-utf8 text))))
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
            (record-command-error-under-lock
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
                    (copy-terminal (managed-terminal session)))
              (condition-notify (attachment-condition attachment)))))))
    (when (and worker
               (not (eq worker (current-thread))))
      (ignore-errors (join-thread worker)))
    t))

(defun run-session-reader (session)
  "Read PTY output in the background for SESSION."
  (let ((termination-condition nil))
    (handler-case
        (loop
          while (session-running-p session)
          do (multiple-value-bind (bytes eof-p)
               (with-lock-held ((session-read-lock session))
                 (read-output-bytes (managed-shell-session session)
                                    :max-bytes +session-read-size+
                                    :wait-p nil))
               (when (and bytes (plusp (length bytes)))
                 (record-session-output session bytes))
               (when eof-p
                 (return))
               (when (or (null bytes) (zerop (length bytes)))
                 (sleep 0.01))))
      (error (condition)
        (setf termination-condition condition)))
    (mark-session-terminated session termination-condition)
    (close-managed-shell-session session)))

(defun dequeue-attachment-output (attachment max-bytes)
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

(defun read-attachment (attachment &key (max-bytes 4096) (wait-p t))
  "Read broadcast PTY bytes from ATTACHMENT."
  (check-type attachment attachment)
  (check-type max-bytes (integer 1))
  (let ((session (attachment-session attachment)))
    (with-lock-held ((session-lock session))
      (loop
        (cond
          ((plusp (attachment-buffer-bytes attachment))
           (return (values (dequeue-attachment-output attachment max-bytes)
                           nil)))
          ((or (not (managed-attachment-attached-p attachment))
               (managed-session-terminated-p session))
           (return (values nil t)))
          ((not wait-p)
           (return (values #() nil)))
          (t
           (condition-wait (attachment-condition attachment)
                          (session-lock session))))))))

(defun input-history (object)
  "Return a copy of the session's newest-first input history."
  (let ((session (etypecase object
                   (managed-session object)
                   (attachment (attachment-session object)))))
    (with-lock-held ((session-lock session))
      (mapcar #'copy-seq (session-input-history session)))))

(defun execution-state (object)
  "Return the current execution state for a session or attachment."
  (let ((session (etypecase object
                   (managed-session object)
                   (attachment (attachment-session object)))))
    (with-lock-held ((session-lock session))
      (if (managed-session-terminated-p session)
          :closed
          (managed-execution-state session)))))

(defun input-draft (attachment)
  "Return ATTACHMENT's private input draft."
  (check-type attachment attachment)
  (let ((session (attachment-session attachment)))
    (with-lock-held ((session-lock session))
      (input-editor-text (attachment-input-editor attachment)))))

(defun input-cursor (attachment)
  "Return ATTACHMENT's input cursor position."
  (check-type attachment attachment)
  (let ((session (attachment-session attachment)))
    (with-lock-held ((session-lock session))
      (input-editor-cursor (attachment-input-editor attachment)))))

(defun set-input-draft (attachment text)
  "Set ATTACHMENT's private input draft."
  (check-type attachment attachment)
  (check-type text string)
  (let ((session (attachment-session attachment)))
    (with-lock-held ((session-lock session))
      (input-editor-set-draft (attachment-input-editor attachment) text)))
  attachment)

(defun valid-input-p (input)
  "Return true when INPUT contains valid transport bytes."
  (or (stringp input)
      (and (vectorp input)
           (every (lambda (byte)
                   (and (integerp byte) (<= 0 byte 255)))
                 input))))

(defun copy-input (input)
  "Return an independent copy of string or octet INPUT."
  (copy-seq input))

(defun submit-input (attachment &optional (input nil input-supplied-p))
  "Submit ATTACHMENT's draft, or explicit INPUT, without interleaving."
  (check-type attachment attachment)
  (when (and input-supplied-p (not (valid-input-p input)))
    (error "Input must be UTF-8 text or an octet vector."))
  (let ((session (attachment-session attachment)))
    (let ((explicit-input (when input-supplied-p
                            (copy-input input))))
      (when input-supplied-p
        (with-lock-held ((session-lock session))
          (when (and (not (command-mode-p session))
                     (stringp explicit-input))
            (input-editor-set-draft
             (attachment-input-editor attachment)
             explicit-input))))
      (when (acquire-lock (session-input-lock session) nil)
        (unwind-protect
             (multiple-value-bind (accepted-p value)
                 (with-lock-held ((session-lock session))
                   (if (not (and (not (command-mode-p session))
                                 (managed-attachment-attached-p attachment)
                                 (session-running-under-lock-p session)))
                       (values nil nil)
                       (values t
                               (if input-supplied-p
                                   explicit-input
                                   (copy-input
                                    (input-editor-text
                                     (attachment-input-editor attachment)))))))
               (when accepted-p
                 (handler-case
                     (progn
                       (with-lock-held ((session-write-lock session))
                         (write-input
                          (managed-shell-session session)
                          value))
                       (with-lock-held ((session-lock session))
                         (input-editor-clear
                          (attachment-input-editor attachment)))
                       t)
                   (error () nil))))
          (release-lock (session-input-lock session)))))))

(defun valid-command-kind-p (kind)
  "Return true when KIND names a command frontend language."
  (member kind '(:lisp :shell) :test #'eq))

(defun submit-command (attachment input kind)
  "Queue one complete command for ATTACHMENT's command frontend."
  (check-type attachment attachment)
  (check-type input string)
  (unless (valid-command-kind-p kind)
    (error "Command kind must be :LISP or :SHELL."))
  (unless (eq kind (input-language input))
    (return-from submit-command nil))
  (unless (member (input-completeness input) '(:complete :error) :test #'eq)
    (return-from submit-command nil))
  (let ((session (attachment-session attachment)))
    (with-lock-held ((session-lock session))
      (let* ((editor (attachment-input-editor attachment))
             (draft (input-editor-text editor)))
        (when (and (command-mode-p session)
                   (managed-attachment-attached-p attachment)
                   (session-running-under-lock-p session)
                   (eq (managed-execution-state session) :ready)
                   (plusp (length input))
                   (= (input-editor-cursor editor) (length draft))
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
          (input-editor-record-submission editor input)
          ;; Clear the accepted draft before the worker can report an error.
          (input-editor-clear editor)
          (condition-notify (execution-condition session))
          t)))))

(defun interrupt-execution (object)
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
                (write-input (managed-shell-session session) (vector 3)))))
          (when (and job-started-p
                     thread
                     (not (eq thread (current-thread))))
            (ignore-errors
              (interrupt-thread
               thread
               (lambda ()
                 (throw +execution-interrupt-tag+ :interrupted))))))
      t)))

(defun next-execution-marker (session)
  "Return an opaque shell marker for SESSION."
  (declare (ignore session))
  (format nil "__LISPORE_COMMAND_DONE_~36R__"
          (random most-positive-fixnum)))

(defun run-shell-command (session attachment input)
  "Run INPUT through SESSION's persistent shell."
  (let ((token (next-execution-marker session)))
    (with-lock-held ((session-lock session))
      (when (session-running-under-lock-p session)
        (setf (execution-marker session) token
              (execution-marker-buffer session) ""
              (execution-shell-input-written-p session) nil
              (execution-attachment session) attachment
              (execution-input session) (copy-seq input))))
    (handler-case
        (progn
          (with-lock-held ((session-write-lock session))
            (write-input
             (managed-shell-session session)
             ;; The wrapper prints a private marker with the command status.
             (format nil "~A~%command printf '~A:%d\\n' $?~%"
                     input
                     token))
            ;; Send Ctrl-C after the command reaches the PTY.
            (with-lock-held ((session-lock session))
              (when (and (session-running-under-lock-p session)
                         (string= token (execution-marker session)))
                (setf (execution-shell-input-written-p session) t)
                (when (execution-interrupted-p session)
                  (ignore-errors
                    (write-input
                     (managed-shell-session session)
                     (vector 3))))))
          (with-lock-held ((session-lock session))
            (loop while (and (session-running-under-lock-p session)
                             (eq (managed-execution-state session) :running))
                  do (condition-wait (execution-condition session)
                                     (session-lock session))))))
      (error (condition)
        (with-lock-held ((session-lock session))
          (record-command-error-under-lock
           attachment
           input
           condition)
          (finish-execution-under-lock session condition))))))

(defun evaluate-lisp-command (session input)
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

(defun run-lisp-command (session attachment input)
  "Evaluate INPUT and publish its output."
  (let ((result
          (catch +execution-interrupt-tag+
            (handler-case
                (progn
                  (let ((output (evaluate-lisp-command session input)))
                    (publish-session-output session output))
                  :finished)
              (error (condition)
                (with-lock-held ((session-lock session))
                  (record-command-error-under-lock attachment input condition))
                (publish-session-output
                 session
                 (format nil "Error: ~A~%" condition))
                :error)))))
    (when (eq result :interrupted)
      (publish-session-output
       session
       (format nil "Execution interrupted.~%"))))
  (with-lock-held ((session-lock session))
    (finish-execution-under-lock session)))

(defun run-execution-job (session job)
  "Run one queued command JOB."
  (destructuring-bind (attachment input kind) job
    (let ((result
            (catch +execution-interrupt-tag+
              (let ((run-p
                      (with-lock-held ((session-lock session))
                        (if (execution-interrupted-p session)
                            nil
                            (progn
                              (setf (execution-job-started-p session) t)
                              t)))))
                (if run-p
                    (ecase kind
                      (:lisp (run-lisp-command session attachment input))
                      (:shell (run-shell-command session attachment input)))
                    (progn
                      (publish-session-output
                       session
                       (format nil "Execution interrupted.~%"))
                      (with-lock-held ((session-lock session))
                        (finish-execution-under-lock session)))))
              :finished)))
      (when (eq result :interrupted)
        (publish-session-output
         session
         (format nil "Execution interrupted.~%"))
        (with-lock-held ((session-lock session))
          (finish-execution-under-lock session))))))

(defun run-execution-worker (session)
  "Serve SESSION's serialized command queue."
  (loop
    do (let ((job
               (with-lock-held ((session-lock session))
                 (loop
                   (when (execution-stop-p session)
                     (return-from run-execution-worker nil))
                   (when (execution-queue session)
                     (return (pop (execution-queue session))))
                   (condition-wait (execution-condition session)
                                  (session-lock session))))))
         (handler-case
             (run-execution-job session job)
           (error (condition)
             (with-lock-held ((session-lock session))
               (record-command-error-under-lock
                (first job)
                (second job)
                condition)
               (finish-execution-under-lock session condition)))))))

(defun terminate-managed-session (session)
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
    (ignore-errors (interrupt-execution session))
    (close-managed-shell-session session)
    (when (and thread
               (not (eq thread (current-thread))))
      (ignore-errors (join-thread thread)))
    (mark-session-terminated session)
    (when (and worker
               (not (eq worker (current-thread))))
      (ignore-errors (join-thread worker)))
    (when (managed-lisp-package session)
      (ignore-errors (delete-package (managed-lisp-package session)))
      (setf (managed-lisp-package session) nil))
    t))

(defun terminate-session (manager session-id)
  "Terminate the session registered under SESSION-ID."
  (let ((session (lookup-session manager session-id)))
    (when session
      (terminate-managed-session session))))

(defun close-session-manager (manager)
  "Terminate every session and stop the in-process registry."
  (check-type manager session-manager)
  (let ((sessions nil)
        (cleanup-thread nil))
    (with-lock-held ((manager-lock manager))
      (setf (manager-closed-p manager) t)
      (setf cleanup-thread (manager-cleanup-thread manager))
      (maphash (lambda (id session)
                 (declare (ignore id))
                 (push session sessions))
               (manager-sessions manager)))
    (mapc #'terminate-managed-session sessions)
    (when (and cleanup-thread
               (not (eq cleanup-thread (current-thread))))
      (ignore-errors (join-thread cleanup-thread)))
    (with-lock-held ((manager-lock manager))
      (clrhash (manager-sessions manager)))
    t))
