(in-package #:lispore.session)

(defconstant +default-retention-seconds+ 300)
(defconstant +default-buffer-bytes+ (* 1024 1024))
(defconstant +session-read-size+ 4096)
(defconstant +session-cleanup-interval+ 0.1)

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
    :accessor session-pending-bytes))
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
   (input-draft-value
    :initform ""
    :accessor attachment-input-draft-value))
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
                                (height 24))
  "Start a fixed-size shell and return its opaque registry ID."
  (check-type manager session-manager)
  (check-type width (integer 1))
  (check-type height (integer 1))
  (let* ((shell-session (start-shell :shell shell
                                     :width width
                                     :height (shell-height height)))
         (managed-session nil)
         (session-id nil)
         (reader-started-p nil))
    (unwind-protect
         (progn
           (with-lock-held ((manager-lock manager))
             (when (manager-closed-p manager)
               (error "The shell session manager is closed."))
             (setf session-id (next-session-id manager)
                   managed-session
                   (make-instance 'managed-session
                                  :id session-id
                                  :manager manager
                                  :shell-session shell-session
                                  :terminal (make-session-terminal width height))
                   (gethash session-id (manager-sessions manager))
                   managed-session)
             (setf (session-reader-thread managed-session)
                   (make-thread
                    (lambda () (run-session-reader managed-session))
                    :name session-id)
                   reader-started-p t))
           session-id)
      (unless reader-started-p
        (when managed-session
          (with-lock-held ((manager-lock manager))
            (when (eq managed-session
                      (gethash session-id (manager-sessions manager)))
              (remhash session-id (manager-sessions manager)))))
        (close-session shell-session)))))

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
  (member mode '(:passthrough :emulated) :test #'eq))

(defun attach-session (manager session-id &key (mode :emulated))
  "Attach a frontend to a running session and return its attachment."
  (check-type manager session-manager)
  (unless (valid-attachment-mode-p mode)
    (error "Attachment mode must be :PASSTHROUGH or :EMULATED."))
  (let ((session (lookup-session manager session-id)))
    (when session
      (with-lock-held ((session-lock session))
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

(defun record-session-output (session bytes)
  "Update SESSION's screen and broadcast BYTES to attachments."
  (with-lock-held ((session-lock session))
    (when (session-running-under-lock-p session)
      (multiple-value-bind (text pending)
          (decode-utf8-chunk bytes (session-pending-bytes session))
        (setf (session-pending-bytes session) pending)
        (when (plusp (length text))
          (feed-terminal (managed-terminal session) text)))
      (dolist (attachment (copy-list (session-attachments session)))
        (unless (append-attachment-output attachment bytes)
          (remove-attachment attachment))))))

(defun mark-session-terminated (session &optional condition)
  "Mark SESSION terminated and wake its attached frontends."
  (with-lock-held ((session-lock session))
    (unless (managed-session-terminated-p session)
      (setf (managed-session-running-p session) nil
            (managed-session-terminated-p session) t
            (session-retention-deadline session)
            (+ (get-internal-real-time)
               (round (* (manager-retention-seconds (session-manager session))
                          internal-time-units-per-second)))
            (managed-session-error session) condition)
      (let ((attachments (session-attachments session)))
        (setf (session-attachments session) nil)
        (dolist (attachment attachments)
          (setf (managed-attachment-attached-p attachment) nil
                (attachment-final-screen attachment)
                (copy-terminal (managed-terminal session)))
          (condition-notify (attachment-condition attachment)))))))

(defun run-session-reader (session)
  "Read PTY output in the background for SESSION."
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
      (mark-session-terminated session condition)))
  (mark-session-terminated session)
  (close-managed-shell-session session))

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

(defun input-draft (attachment)
  "Return ATTACHMENT's private input draft."
  (check-type attachment attachment)
  (let ((session (attachment-session attachment)))
    (with-lock-held ((session-lock session))
      (copy-seq (attachment-input-draft-value attachment)))))

(defun set-input-draft (attachment text)
  "Set ATTACHMENT's private input draft."
  (check-type attachment attachment)
  (check-type text string)
  (let ((session (attachment-session attachment)))
    (with-lock-held ((session-lock session))
      (setf (attachment-input-draft-value attachment) (copy-seq text))))
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
          (setf (attachment-input-draft-value attachment)
                explicit-input)))
      (when (acquire-lock (session-input-lock session) nil)
        (unwind-protect
             (with-lock-held ((session-lock session))
               (if (not (and (managed-attachment-attached-p attachment)
                             (session-running-under-lock-p session)))
                   nil
                   (let ((value (if input-supplied-p
                                    explicit-input
                                    (copy-input
                                     (attachment-input-draft-value attachment)))))
                     (handler-case
                         (progn
                           (with-lock-held ((session-write-lock session))
                             (write-input (managed-shell-session session) value))
                           (when (equalp value
                                        (attachment-input-draft-value attachment))
                             (setf (attachment-input-draft-value attachment) ""))
                           t)
                       (error () nil)))))
          (release-lock (session-input-lock session)))))))

(defun terminate-managed-session (session)
  "Terminate SESSION and wait for its reader thread."
  (let ((thread nil))
    (with-lock-held ((session-lock session))
      (setf (managed-session-running-p session) nil
            thread (session-reader-thread session)))
    (close-managed-shell-session session)
    (when (and thread
               (not (eq thread (current-thread))))
      (ignore-errors (join-thread thread)))
    (mark-session-terminated session)
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
