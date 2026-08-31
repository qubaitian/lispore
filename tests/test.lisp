(in-package #:lispore.tests)

(defvar *tests* nil)

(defmacro deftest (name () &body body)
  `(progn
     (defun ,name () ,@body)
     (pushnew ',name *tests*)))

(defun check (condition format-control &rest format-arguments)
  (unless condition
    (error (apply #'format nil format-control format-arguments))))

(defun screen-has-text-p (terminal text)
  (some (lambda (line) (search text line))
        (get-terminal-screen-lines terminal)))

(defun wait-until (predicate &key (attempts 100) (delay 0.05))
  (loop repeat attempts
        when (funcall predicate)
          do (return t)
        do (sleep delay)
        finally (return (funcall predicate))))

(defun attachment-output-has-text-p (attachment text)
  (let ((output ""))
    (loop repeat 100
          do (multiple-value-bind (bytes eof-p)
                 (get-attachment-output attachment :wait-p nil)
               (when (and bytes (plusp (length bytes)))
                 (setf output
                       (concatenate 'string
                                    output
                                    (map 'string #'code-char bytes)))
                 (when (search text output)
                   (return-from attachment-output-has-text-p t)))
               (when eof-p
                 (return nil))
               (sleep 0.01)))
    nil))

(defun drain-attachment (attachment)
  "Remove all currently buffered output from ATTACHMENT."
  (loop
    for bytes = (get-attachment-output attachment :wait-p nil)
    while (and bytes (plusp (length bytes)))))

(deftest diagnostic-logger-keeps-complete-events ()
  (let ((stream (make-string-output-stream))
        (records nil)
        (records-lock (make-lock "diagnostic logger test")))
    (let ((logger
            (new-diagnostic-logger
             stream
             (lambda (record)
               (with-lock-held (records-lock)
                 (push record records))))))
      (unwind-protect
           (progn
             (check (set-diagnostic-event
                     logger
                     "command-error"
                     :session-id "session-1"
                     :session-name "s2"
                     :message "evaluation failed"
                     :input "(error \"boom\")"
                     :condition
                     (make-condition 'simple-error
                                     :format-control "boom"
                                     :format-arguments nil))
                    "The diagnostic logger rejects a complete event.")
             (check (wait-until
                     (lambda ()
                       (with-lock-held (records-lock)
                         (not (null records)))))
                    "The diagnostic logger does not broadcast its event.")
             (let ((text (get-output-stream-string stream)))
               (check (search "event=command-error" text)
                      "The diagnostic log omits the event name.")
               (check (search "session-name=s2" text)
                      "The diagnostic log omits the session name.")
               (check (search "input-begin" text)
                      "The diagnostic log omits submitted input.")
               (check (search "(error \"boom\")" text)
                      "The diagnostic log truncates submitted input.")
               (check (search "backtrace-begin" text)
                      "The diagnostic log omits the SBCL backtrace.")))
        (del-diagnostic-logger logger)))))

(deftest command-errors-keep-running-and-write-diagnostics ()
  (let ((manager (new-session-manager :retention-seconds 5))
        (stream (make-string-output-stream)))
    (let ((logger
            (new-diagnostic-logger
             stream
             (lambda (record)
               (declare (ignore record))))))
      (check (eq logger (set-session-manager-logger manager logger))
             "The manager rejects its diagnostic logger.")
      (unwind-protect
           (let* ((session-id (new-session manager
                                              :shell "/bin/sh"
                                              :mode :command
                                              :width 40
                                              :height 6))
                  (session (get-session manager session-id))
                  (attachment (set-current-session manager
                                               session-id
                                               :mode :command)))
             (set-input-draft attachment "(error \"boom\")")
             (check (set-command-submission attachment
                                     (get-input-draft attachment)
                                     :lisp)
                    "The command frontend rejects an error form.")
             (set-input-draft attachment "")
             (check (wait-until
                     (lambda ()
                       (and (eq :ready (get-execution-state session))
                            (screen-has-text-p (get-attachment-screen attachment)
                                               "Error: boom"))))
                    "The SBCL error does not return the session to ready.")
             (let ((text (get-output-stream-string stream)))
               (check (search "event=command-error" text)
                      "The command error is absent from diagnostics.")
               (check (search "(error \"boom\")" text)
                      "The diagnostic input is incomplete.")
               (check (search "backtrace-begin" text)
                      "The command error has no backtrace.")))
        (del-session-manager manager)))))

(deftest diagnostic-output-starts-each-line-at-column-zero ()
  (let ((manager (new-session-manager :retention-seconds 5)))
    (unwind-protect
         (let* ((session-id (new-session manager
                                           :shell "/bin/sh"
                                           :mode :command
                                           :width 40
                                           :height 8))
                (session (get-session manager session-id))
                (attachment (set-current-session manager
                                                  session-id
                                                  :mode :command))
                (text (format nil
                              "~%[lispore debug]~%record-begin~%timestamp=now~%record-end~%")))
           (set-session-published-output session text)
           (let ((lines (get-terminal-screen-lines
                         (get-attachment-screen attachment))))
             (check (and (eql 0 (search "[lispore debug]" (second lines)))
                         (eql 0 (search "record-begin" (third lines)))
                         (eql 0 (search "timestamp=now" (fourth lines)))
                         (eql 0 (search "record-end" (fifth lines))))
                    "Diagnostic lines do not start at column zero.")))
      (del-session-manager manager))))

(defun make-test-pipe ()
  "Return readable and writable descriptors for a test pipe."
  (cffi:with-foreign-object (descriptors :int 2)
    (check (zerop (cffi:foreign-funcall "pipe"
                                       :pointer descriptors
                                       :int))
           "The test pipe cannot be created.")
    (values (cffi:mem-aref descriptors :int 0)
            (cffi:mem-aref descriptors :int 1))))

(defun drain-test-pipe (read-descriptor)
  "Read all available text from READ-DESCRIPTOR without blocking."
  (with-output-to-string (output)
    (loop for events = (get-poll-events (list (cons read-descriptor +pollin+))
                                 :timeout 0)
          while events
          do (multiple-value-bind (bytes eof-p)
                 (get-fd read-descriptor :wait-p nil)
               (when (and bytes (plusp (length bytes)))
                 (write-string (map 'string #'code-char bytes) output))
               (when eof-p
               (return))))))

(deftest input-editor-inserts-at-the-cursor ()
  (let ((editor (new-input-editor)))
    (set-input-editor-paste editor "abc")
    (set-input-editor-bytes editor
                       (get-utf8 (format nil "~C[D" #\Escape)))
    (set-input-editor-bytes editor (get-utf8 "X"))
    (check (string= "abXc" (get-input-editor-text editor))
           "The input editor does not insert at the cursor.")
    (check (= 3 (get-input-editor-cursor editor))
           "The input editor cursor has the wrong position.")))

(deftest input-editor-supports-editing-keys ()
  (let ((editor (new-input-editor)))
    (set-input-editor-paste editor "abcd")
    (set-input-editor-bytes editor
                       (get-utf8 (format nil "~C[H" #\Escape)))
    (set-input-editor-bytes editor (get-utf8 "X"))
    (set-input-editor-bytes editor
                       (get-utf8 (format nil "~C[3~~" #\Escape)))
    (check (string= "Xbcd" (get-input-editor-text editor))
           "The input editor does not support Home or Delete.")
    (set-input-editor-bytes editor
                       (get-utf8 (format nil "~C[F" #\Escape)))
    (set-input-editor-bytes editor (vector #x7f))
    (check (string= "Xbc" (get-input-editor-text editor))
           "The input editor does not support End or Backspace.")))

(deftest input-editor-controls-work-in-escape-sequences ()
  (let ((editor (new-input-editor)))
    (set-input-editor-bytes editor (get-utf8 "draft"))
    (set-input-editor-bytes editor (vector #xe2))
    (let ((events (set-input-editor-bytes editor (vector 3))))
      (check (and (= 1 (length events))
                  (eq :interrupt (input-event-type (first events))))
             "Control-C cannot follow an incomplete UTF-8 sequence."))
    (del-input-editor editor)
    (set-input-editor-bytes editor (vector #xe2))
    (let ((events (set-input-editor-bytes editor (vector 4))))
      (check (and (= 1 (length events))
                  (eq :eof (input-event-type (first events))))
             "Control-D cannot follow an incomplete UTF-8 sequence."))
    (let ((events
            (set-input-editor-bytes
             editor
             (get-utf8 (format nil "~C[~C" #\Escape (code-char 3))))))
      (check (and (= 1 (length events))
                  (eq :interrupt (input-event-type (first events))))
             "Control-C does not interrupt an escape sequence.")
      (del-input-editor editor)
      (let ((events
              (set-input-editor-bytes
               editor
               (get-utf8 (format nil "~C[~C" #\Escape (code-char 4))))))
        (check (and (= 1 (length events))
                    (eq :eof (input-event-type (first events))))
               "Control-D does not close an empty escape sequence.")))))

(deftest input-editor-preserves-utf8-and-paste-newlines ()
  (let* ((editor (new-input-editor))
         (bytes (get-utf8 "界")))
    (set-input-editor-bytes editor (subseq bytes 0 2))
    (check (string= "" (get-input-editor-text editor))
           "The input editor emits a split UTF-8 character.")
    (set-input-editor-bytes editor (subseq bytes 2))
    (check (string= "界" (get-input-editor-text editor))
           "The input editor loses a split UTF-8 character.")
    (del-input-editor editor)
    (set-input-editor-bytes
     editor
     (get-utf8 (format nil "~C[200~~one~%two~C[201~~"
                          #\Escape
                          #\Escape)))
    (check (string= (format nil "one~%two") (get-input-editor-text editor))
           "The input editor does not preserve multiline paste.")
    (let ((events (set-input-editor-bytes editor (vector 13))))
      (check (and (= 1 (length events))
                  (eq :enter (input-event-type (first events)))
                  (string= (format nil "one~%two")
                           (input-event-text (first events))))
             "The input editor does not emit an Enter event."))))

(deftest input-editor-navigates-recovery-before-get-input-history ()
  (let ((editor (new-input-editor)))
    (set-input-editor-history editor (list "second" "first"))
    (set-input-editor-draft-history editor "error draft")
    (set-input-editor-paste editor "current")
    (set-input-editor-bytes editor
                       (get-utf8 (format nil "~C[A" #\Escape)))
    (check (string= "error draft" (get-input-editor-text editor))
           "The input editor skips the recovery draft.")
    (set-input-editor-bytes editor
                       (get-utf8 (format nil "~C[A" #\Escape)))
    (check (string= "second" (get-input-editor-text editor))
           "The input editor skips input history.")
    (set-input-editor-bytes editor
                       (get-utf8 (format nil "~C[B" #\Escape)))
    (check (string= "error draft" (get-input-editor-text editor))
           "The input editor cannot navigate back to the recovery draft.")
    (set-input-editor-bytes editor
                       (get-utf8 (format nil "~C[B" #\Escape)))
    (check (string= "current" (get-input-editor-text editor))
           "The input editor does not restore the current draft.")
    (set-input-editor-bytes editor (get-utf8 "X"))
    (set-input-editor-submission editor (get-input-editor-text editor))
    (set-input-editor-bytes editor
                       (get-utf8 (format nil "~C[A" #\Escape)))
    (set-input-editor-bytes editor
                       (get-utf8 (format nil "~C[A" #\Escape)))
    (check (string= "currentX" (get-input-editor-text editor))
           "Editing recalled input does not create a history entry.")))

(deftest command-session-evaluates-persistent-lisp-forms ()
  (let ((manager (new-session-manager :retention-seconds 5)))
    (unwind-protect
        (let* ((session-id (new-session manager
                                          :shell "/bin/sh"
                                          :mode :command
                                          :width 40
                                          :height 6))
               (session (get-session manager session-id))
               (attachment (set-current-session manager
                                            session-id
                                            :mode :command)))
          (set-input-draft attachment "(defparameter *input-marker* 41)")
          (check (set-command-submission attachment
                                 (get-input-draft attachment)
                                 :lisp)
                 "The command frontend rejects a Lisp form.")
          (set-input-draft attachment "")
          (check (wait-until (lambda ()
                               (eq :ready (get-execution-state session))))
                 "The Lisp execution does not finish.")
          (set-input-draft attachment "(1+ *input-marker*)")
          (check (set-command-submission attachment
                                 (get-input-draft attachment)
                                 :lisp)
                 "The command frontend rejects its second Lisp form.")
          (set-input-draft attachment "")
          (check (wait-until
                  (lambda ()
                    (and (eq :ready (get-execution-state session))
                         (screen-has-text-p (get-attachment-screen attachment)
                                            "42"))))
                 "The Lisp package does not persist between forms.")
          (check (= 2 (length (get-input-history session)))
                 "The session does not retain submitted input history."))
      (del-session-manager manager))))

(deftest session-defaults-to-command-frontend-and-rejects-unsupported-mode ()
  (let ((manager (new-session-manager :retention-seconds 5)))
    (unwind-protect
        (let* ((session-id (new-session manager :shell "/bin/sh"))
               (attachment (set-current-session manager session-id))
               (new-error-p nil)
               (set-error-p nil))
          (check (eq :command (attachment-mode attachment))
                 "A session does not default to the command frontend.")
          (handler-case
              (new-session manager :shell "/bin/sh" :mode :emulated)
            (error () (setf new-error-p t)))
          (handler-case
              (set-current-session manager session-id :mode :emulated)
            (error () (setf set-error-p t)))
          (check new-error-p
                 "A session accepts an unsupported mode.")
          (check set-error-p
                 "An attachment accepts an unsupported mode."))
      (del-session-manager manager))))

(deftest named-sessions-new-rejects-duplicates-and-reports-errors ()
  (let ((manager (new-session-manager :retention-seconds 5)))
    (unwind-protect
        (let* ((first-id (new-session manager
                                       :name "s1"
                                       :shell "/bin/sh"
                                       :width 40
                                       :height 6))
               (first (get-session manager first-id))
               (duplicate-error-p nil)
               (attachment (set-current-session manager
                                            (session-id first)
                                            :mode :command)))
          (handler-case
              (new-session manager :name "s1" :shell "/bin/sh")
            (error () (setf duplicate-error-p t)))
          (check duplicate-error-p
                 "A repeated name creates a second session.")
          (check (string= "s1" (session-name first))
                 "A named session loses its name.")
          (check (eq first (get-session-by-name manager "s1"))
                 "Name lookup does not return the named session.")
          (check (equal '("s1" . :ready)
                        (first (get-session-list manager)))
                 "The session list omits the ready named session.")
          (set-input-draft attachment "false")
          (check (set-command-submission attachment "false" :shell)
                 "The named session rejects a shell command.")
          (check (wait-until
                  (lambda ()
                    (equal '("s1" . :error)
                           (first (get-session-list manager)))))
                 "A failed command does not report error state.")
          (set-input-draft attachment "(error \"bad\")")
          (check (set-command-submission attachment "(error \"bad\")" :lisp)
                 "The named session rejects a Lisp command.")
          (check (wait-until
                  (lambda ()
                    (equal '("s1" . :error)
                           (first (get-session-list manager)))))
                 "A failed Lisp command does not report error state.")
          (set-input-draft attachment "true")
          (check (set-command-submission attachment "true" :shell)
                 "The named session rejects its next command.")
          (check (wait-until
                  (lambda ()
                    (equal '("s1" . :ready)
                           (first (get-session-list manager)))))
                 "A later command does not clear error state."))
      (del-session-manager manager))))

(deftest session-names-reject-paths ()
  (let ((manager (new-session-manager :retention-seconds 5))
        (raised-p nil))
    (unwind-protect
        (handler-case
            (new-session manager :name "bad/name" :shell "/bin/sh")
          (error () (setf raised-p t)))
      (del-session-manager manager))
    (check raised-p
           "A session name accepts path separators.")))

(deftest session-modes-reject-incompatible-attachments ()
  (let ((manager (new-session-manager :retention-seconds 5)))
    (unwind-protect
        (let ((command-id (new-session manager
                                         :shell "/bin/sh"
                                         :mode :command))
              (passthrough-id (new-session manager
                                             :shell "/bin/sh"
                                             :mode :passthrough))
              (command-error-p nil)
              (passthrough-error-p nil))
          (handler-case
              (set-current-session manager command-id :mode :passthrough)
            (error () (setf command-error-p t)))
          (handler-case
              (set-current-session manager passthrough-id :mode :command)
            (error () (setf passthrough-error-p t)))
          (check command-error-p
                 "A command session accepts passthrough mode.")
          (check passthrough-error-p
                 "A passthrough session accepts command mode."))
      (del-session-manager manager))))

(deftest command-session-rejects-mismatched-language ()
  (let ((manager (new-session-manager :retention-seconds 5)))
    (unwind-protect
        (let* ((session-id (new-session manager
                                          :shell "/bin/sh"
                                          :mode :command))
               (attachment (set-current-session manager
                                            session-id
                                            :mode :command)))
          (set-input-draft attachment "(+ 1 2)")
          (check (not (set-command-submission attachment
                                       (get-input-draft attachment)
                                       :shell))
                 "The API accepts a Lisp draft as a shell command.")
          (check (string= "(+ 1 2)" (get-input-draft attachment))
                 "A mismatched language changes the frontend input."))
      (del-session-manager manager))))

(deftest command-session-runs-shell-commands-in-order ()
  (let ((manager (new-session-manager :retention-seconds 5)))
    (unwind-protect
        (let* ((session-id (new-session manager
                                          :shell "/bin/sh"
                                          :mode :command
                                          :width 40
                                          :height 6))
               (session (get-session manager session-id))
               (attachment (set-current-session manager
                                            session-id
                                            :mode :command)))
          (set-input-draft attachment "printf 'command-marker\\n'")
          (check (set-command-submission attachment
                                 (get-input-draft attachment)
                                 :shell)
                 "The command frontend rejects a shell command.")
          (set-input-draft attachment "")
          (check (wait-until
                  (lambda ()
                    (and (eq :ready (get-execution-state session))
                         (screen-has-text-p (get-attachment-screen attachment)
                                            "command-marker"))))
                 "The shell command does not finish visibly.")
          (set-input-draft attachment "printf() { :; }; echo function-marker")
          (check (set-command-submission attachment
                                 (get-input-draft attachment)
                                 :shell)
                 "The marker test rejects a shell function definition.")
          (set-input-draft attachment "")
          (check (wait-until
                 (lambda ()
                    (and (eq :ready (get-execution-state session))
                         (screen-has-text-p (get-attachment-screen attachment)
                                            "function-marker"))))
                 "The shell marker ignores a redefined printf.")
          (set-input-draft attachment "command printf 'line-one\\n'")
          (check (set-command-submission attachment
                                 (get-input-draft attachment)
                                 :shell)
                 "The line-ending test rejects its shell command.")
          (set-input-draft attachment "")
          (check (wait-until
                  (lambda ()
                    (and (eq :ready (get-execution-state session))
                         (screen-has-text-p
                          (get-attachment-screen attachment)
                          "line-one"))))
                 "The line-ending test command does not finish.")
          (set-input-draft attachment "command printf line-two")
          (check (set-command-submission attachment
                                 (get-input-draft attachment)
                                 :shell)
                 "The adjacent-output test rejects its shell command.")
          (set-input-draft attachment "")
          (check (wait-until
                  (lambda ()
                    (and (eq :ready (get-execution-state session))
                         (let ((lines (get-terminal-screen-lines
                                       (get-attachment-screen attachment))))
                           (let ((line-one-index
                                   (position-if
                                    (lambda (line)
                                      (search "line-one" line))
                                    lines))
                                 (line-two-index
                                   (position-if
                                    (lambda (line)
                                      (search "line-two" line))
                                    lines)))
                             (and line-one-index
                                  line-two-index
                                  (> line-two-index line-one-index)))))))
                 "The shell marker changes command output line endings."))
      (del-session-manager manager))))

(deftest command-session-restores-a-shell-error-draft ()
  (let ((manager (new-session-manager :retention-seconds 5)))
    (unwind-protect
        (let* ((session-id (new-session manager
                                          :shell "/bin/sh"
                                          :mode :command))
               (session (get-session manager session-id))
               (attachment (set-current-session manager
                                            session-id
                                            :mode :command))
               (input "false"))
          (set-input-draft attachment input)
          (check (set-command-submission attachment input :shell)
                 "The shell failure test input does not start.")
          (check (wait-until
                  (lambda ()
                    (and (eq :ready (get-execution-state session))
                         (string= input (get-input-draft attachment)))))
                 "The command frontend does not restore a shell error draft.")
          (check (string= input (first (get-input-history session)))
                 "The failed shell command does not enter input history."))
      (del-session-manager manager))))

(deftest command-session-ignores-forged-marker-text ()
  (let ((manager (new-session-manager :retention-seconds 5)))
    (unwind-protect
        (let* ((session-id (new-session manager
                                          :shell "/bin/sh"
                                          :mode :command))
               (session (get-session manager session-id))
               (attachment (set-current-session manager
                                            session-id
                                            :mode :command))
               (input "printf '__LISPORE_COMMAND_DONE_1__:0\\n'"))
          (set-input-draft attachment input)
          (check (set-command-submission attachment input :shell)
                 "The forged marker test input does not start.")
          (check (wait-until
                  (lambda ()
                    (and (eq :ready (get-execution-state session))
                         (screen-has-text-p
                          (get-attachment-screen attachment)
                          "__LISPORE_COMMAND_DONE_1__"))))
                 "Command output can forge the execution marker."))
      (del-session-manager manager))))

(deftest command-session-restores-a-draft-before-shell-termination ()
  (let ((manager (new-session-manager :retention-seconds 5)))
    (unwind-protect
        (let* ((session-id (new-session manager
                                          :shell "/bin/sh"
                                          :mode :command))
               (session (get-session manager session-id))
               (attachment (set-current-session manager
                                            session-id
                                            :mode :command))
               (input "exit 7"))
          (set-input-draft attachment input)
          (check (set-command-submission attachment input :shell)
                 "The shell termination test input does not start.")
          (check (wait-until
                  (lambda ()
                    (and (eq :closed (get-execution-state session))
                         (string= input (get-input-draft attachment)))))
                 "Shell termination loses the active command draft."))
      (del-session-manager manager))))

(deftest command-session-stops-worker-after-shell-exit ()
  (let ((manager (new-session-manager :retention-seconds 5)))
    (unwind-protect
        (let* ((session-id (new-session manager
                                          :shell "/bin/sh"
                                          :mode :command))
               (session (get-session manager session-id))
               (attachment (set-current-session manager
                                            session-id
                                            :mode :command)))
          (set-input-draft attachment "(sleep 5)")
          (check (set-command-submission attachment
                                 (get-input-draft attachment)
                                 :lisp)
                 "The worker shutdown test input does not start.")
          (check (wait-until
                  (lambda ()
                    (eq :running (get-execution-state session))))
                 "The worker shutdown test does not start execution.")
          (set-shell-input (lispore.session::managed-shell-session session)
                       (format nil "exit~%"))
          (check (wait-until
                  (lambda ()
                    (and (eq :closed (get-execution-state session))
                         (not (thread-alive-p
                               (lispore.session::execution-thread session))))))
                 "Shell exit leaves the Lisp execution worker alive."))
      (del-session-manager manager))))

(deftest command-session-rejects-trailing-lisp-text ()
  (let ((manager (new-session-manager :retention-seconds 5)))
    (unwind-protect
        (let* ((session-id (new-session manager
                                          :shell "/bin/sh"
                                          :mode :command))
               (session (get-session manager session-id))
               (attachment (set-current-session manager
                                            session-id
                                            :mode :command))
               (input "(+ 1 2) echo ready"))
          (set-input-draft attachment input)
          (check (set-command-submission attachment input :lisp)
                 "The mixed Lisp input does not start.")
          (check (wait-until
                  (lambda ()
                    (and (eq :ready (get-execution-state session))
                         (string= input (get-input-draft attachment)))))
                 "The command frontend drops trailing Lisp text.")
          (check (string= input (first (get-input-history session)))
                 "The mixed Lisp input does not enter input history."))
      (del-session-manager manager))))

(deftest command-session-restores-an-evaluation-error-draft ()
  (let ((manager (new-session-manager :retention-seconds 5)))
    (unwind-protect
        (let* ((session-id (new-session manager
                                          :shell "/bin/sh"
                                          :mode :command))
               (session (get-session manager session-id))
               (attachment (set-current-session manager
                                            session-id
                                            :mode :command))
               (input "(error \"expected failure\")"))
          (set-input-draft attachment input)
          (check (set-command-submission attachment input :lisp)
                 "The command frontend rejects an error form.")
          (set-input-draft attachment "")
          (check (wait-until
                  (lambda ()
                    (and (eq :ready (get-execution-state session))
                         (string= input (get-input-draft attachment)))))
                 "The command frontend does not restore the error draft.")
          (check (string= input (first (get-input-history session)))
                 "The failed form does not enter input history."))
      (del-session-manager manager))))

(deftest command-session-rejects-submission-during-active-execution ()
  (let ((manager (new-session-manager :retention-seconds 5)))
    (unwind-protect
        (let* ((session-id (new-session manager
                                          :shell "/bin/sh"
                                          :mode :command))
               (session (get-session manager session-id))
               (attachment (set-current-session manager
                                            session-id
                                            :mode :command)))
          (set-input-draft attachment "(progn (sleep 0.4) 7)")
          (check (set-command-submission attachment
                                 (get-input-draft attachment)
                                 :lisp)
                 "The active command does not start.")
          (check (string= "" (get-input-draft attachment))
                 "Accepted input remains in the frontend input.")
          (set-input-draft attachment "(+ 1 2)")
          (check (not (set-command-submission attachment
                                       (get-input-draft attachment)
                                       :lisp))
                 "The command frontend submits during active execution.")
          (check (not (set-input-submission attachment
                                    (get-utf8 (format nil "echo raw~%"))))
                 "Raw input bypasses the command execution queue.")
          (check (string= "(+ 1 2)" (get-input-draft attachment))
                 "Rejected input changes the frontend input.")
          (check (wait-until
                  (lambda ()
                    (and (eq :ready (get-execution-state session))
                         (screen-has-text-p (get-attachment-screen attachment)
                                            "7"))))
                 "The active command does not finish."))
      (del-session-manager manager))))

(deftest command-session-interrupts-lisp-execution ()
  (let ((manager (new-session-manager :retention-seconds 5)))
    (unwind-protect
        (let* ((session-id (new-session manager
                                          :shell "/bin/sh"
                                          :mode :command))
               (session (get-session manager session-id))
               (attachment (set-current-session manager
                                            session-id
                                            :mode :command)))
          (set-input-draft attachment "(loop (sleep 1))")
          (check (set-command-submission attachment
                                 (get-input-draft attachment)
                                 :lisp)
                 "The interrupt test input does not start.")
          (check (wait-until
                  (lambda () (eq :running (get-execution-state session))))
                 "The interrupt test does not enter active execution.")
          (check (set-execution-interruption attachment)
                 "The command frontend does not accept an interrupt.")
          (check (wait-until
                  (lambda ()
                    (and (eq :ready (get-execution-state session))
                         (screen-has-text-p
                          (get-attachment-screen attachment)
                          "Execution interrupted"))))
                 "The Lisp execution does not stop after Ctrl-C.")
          (set-input-draft attachment "(+ 4 5)")
          (check (set-command-submission attachment
                                 (get-input-draft attachment)
                                 :lisp)
                 "The worker does not accept input after an interrupt.")
          (check (wait-until
                  (lambda ()
                    (and (eq :ready (get-execution-state session))
                         (screen-has-text-p (get-attachment-screen attachment)
                                            "9"))))
                 "The worker does not continue after an interrupt."))
      (del-session-manager manager))))

(deftest command-input-selects-language-and-completeness ()
  (check (eq :lisp (get-input-language "(+ 1 2)"))
         "The command frontend misclassifies a Lisp form.")
  (check (eq :complete
             (get-input-completeness "(+ 1 2)"))
         "The command frontend rejects a complete Lisp form.")
  (check (eq :incomplete
             (get-input-completeness "(+ 1 2"))
         "The command frontend accepts an incomplete Lisp form.")
  (check (eq :complete
             (get-input-completeness "(+ 1 2) echo ready"))
         "The command frontend rejects a mixed shell command.")
  (check (eq :complete
             (get-input-completeness "echo if"))
         "The command frontend misreads a shell argument as syntax.")
  (check (eq :complete
             (get-input-completeness "echo [ if ]"))
         "The command frontend misreads test arguments as syntax.")
  (check (eq :incomplete
             (get-input-completeness "printf 'unfinished"))
         "The command frontend accepts an unfinished shell quote.")
  (check (eq :incomplete
             (get-input-completeness "echo | "))
         "The command frontend accepts a trailing pipe.")
  (check (eq :incomplete
             (get-input-completeness "if true; then"))
         "The command frontend accepts an unfinished shell compound.")
  (check (eq :incomplete
             (get-input-completeness "cat <<EOF"))
         "The command frontend accepts an unfinished here-document.")
  (check (eq :complete
             (get-input-completeness (format nil "cat <<EOF~%if~%EOF")))
         "The command frontend parses here-document bodies as data.")
  (check (eq :complete
             (get-input-completeness
              "if true; then echo ready; fi"))
         "The command frontend rejects a complete shell compound."))

(deftest command-frontend-adds-newlines-to-incomplete-input ()
  (let ((manager (new-session-manager :retention-seconds 5)))
    (unwind-protect
        (let* ((session-id (new-session manager
                                          :shell "/bin/sh"
                                          :mode :command))
               (attachment (set-current-session manager
                                            session-id
                                            :mode :command)))
          (set-input-draft attachment "if true; then")
          (check (not (set-command-submission attachment
                                       (get-input-draft attachment)
                                       :shell))
                 "The command session accepts incomplete input.")
          (check (not (lispore.frontend::set-command-input
                       attachment
                       (get-utf8 (string #\Return))))
                 "Incomplete input closes the command frontend.")
          (check (string= (format nil "if true; then~%")
                          (get-input-draft attachment))
                 "Enter submits incomplete input instead of adding a newline.")
          (check (not (lispore.frontend::set-command-input
                       attachment
                       (vector 4)))
                 "Ctrl-D closes with a non-empty draft.")
          (check (string= (format nil "if true; then~%")
                          (get-input-draft attachment))
                 "Ctrl-D changes a non-empty draft.")
          (set-input-draft attachment "if true; then")
          (lispore.frontend::set-command-input
           attachment
           (get-utf8 (format nil "~C[D~C" #\Escape #\Return)))
          (check (string= (format nil "if true; the~%n")
                          (get-input-draft attachment))
                 "Enter does not insert inside an incomplete draft."))
      (del-session-manager manager))))

(deftest command-frontend-renders-editor-frame ()
  (let ((manager (new-session-manager :retention-seconds 5)))
    (unwind-protect
        (let* ((session-id (new-session manager
                                          :shell "/bin/sh"
                                          :mode :command
                                          :width 20
                                          :height 6))
               (attachment (set-current-session manager
                                            session-id
                                            :mode :command))
               (text (format nil "abc~%def"))
               (frame nil))
          (set-input-draft attachment text)
          (setf frame (lispore.frontend::get-command-frame attachment))
          (check (equal (list "abc" "def")
                        (lispore.frontend::get-input-lines text 20))
                 "The command frontend wraps input into wrong rows.")
          (multiple-value-bind (row column)
              (lispore.frontend::get-input-cursor-row-column text 7 20)
            (check (and (= 1 row) (= 3 column))
                   "The command frontend calculates the wrong cursor."))
          (check (search "session-1" frame)
                 "The command frontend omits the session status.")
          (check (search "abc" frame)
                 "The command frontend omits the first input row.")
          (check (search "def" frame)
                 "The command frontend omits the second input row.")
          (check (search (format nil "~C[2J" #\Escape) frame)
                 "The command frontend does not clear the old frame."))
      (del-session-manager manager))))

(deftest command-frontend-edits-and-submits-input ()
  (let ((manager (new-session-manager :retention-seconds 5)))
    (multiple-value-bind (read-pipe write-pipe)
        (make-test-pipe)
      (unwind-protect
          (let* ((session-id (new-session manager
                                            :shell "/bin/sh"
                                            :mode :command
                                            :width 40
                                            :height 6))
                 (session (get-session manager session-id))
                 (attachment (set-current-session manager
                                              session-id
                                              :mode :command))
                 (finished-p nil)
                 (thread
                   (make-thread
                    (lambda ()
                      (set-command-frontend :attachment attachment
                                   :input-fd read-pipe
                                   :output-fd nil)
                      (setf finished-p t)))))
            (set-fd write-pipe
                      (get-utf8 (format nil "(+ 2 3)~C~C"
                                           #\Return
                                           (code-char 4))))
            (check (wait-until
                    (lambda ()
                      (and (eq :ready (get-execution-state session))
                           (screen-has-text-p
                            (get-attachment-screen attachment)
                            "5"))))
                   "The command frontend does not submit edited input.")
            (join-thread thread)
            (del-pty write-pipe)
            (check finished-p
                   "The command frontend does not stop at input EOF."))
        (del-pty read-pipe)
        (ignore-errors (del-pty write-pipe))
        (del-session-manager manager)))))

(deftest detached-session-restores-get-retained-screen ()
  (let ((manager (new-session-manager :retention-seconds 5)))
    (unwind-protect
        (let* ((session-id (new-session manager
                                          :shell "/bin/sh"
                                          :width 20
                                          :height 4
                                          :mode :passthrough))
               (session (get-session manager session-id)))
          (multiple-value-bind (attachment screen)
              (set-current-session manager session-id :mode :passthrough)
            (check attachment
                   "The session does not accept its first attachment.")
            (check (not (screen-has-text-p screen "session-marker"))
                   "The first attachment returns stale screen data.")
            (set-input-draft attachment
                             (format nil "printf 'session-marker\\n'; sleep 1~%"))
            (check (set-input-submission attachment)
                   "The first input submission is rejected.")
            (check (wait-until
                    (lambda ()
                      (screen-has-text-p (get-attachment-screen attachment)
                            "session-marker")))
                   "The session does not retain shell output.")
            (check (del-current-session attachment)
                   "The frontend cannot del-current-session from the session.")
            (check (get-session-running-p session)
                   "Detachment terminates the shell session.")
            (multiple-value-bind (restored screen)
                (set-current-session manager session-id :mode :passthrough)
              (check restored
                     "The running session cannot be restored.")
              (check (screen-has-text-p screen "session-marker")
                     "Restoration loses the retained screen.")
              (set-input-draft restored
                               (format nil "printf 'restore-marker\\n'; exit~%"))
              (check (set-input-submission restored)
                     "The restored frontend cannot submit input.")
              (check (wait-until
                      (lambda ()
                        (not (get-session-running-p session))))
                     "The restored shell session does not terminate.")
              (check (not (get-attachment-attached-p restored))
                     "Session termination leaves the attachment connected.")
              (check (null (set-current-session manager session-id
                                            :mode :passthrough))
                     "The terminated session accepts a new attachment.")
              (check (screen-has-text-p (get-attachment-screen restored)
                                        "restore-marker")
                     "The attached frontend loses the final screen."))))
      (del-session-manager manager))))

(deftest managed-session-broadcasts-output-to-every-attachment ()
  (let ((manager (new-session-manager :retention-seconds 5)))
    (unwind-protect
        (let ((session-id (new-session manager
                                         :shell "/bin/sh"
                                         :width 20
                                         :height 4
                                         :mode :passthrough)))
          (let ((first (set-current-session manager session-id :mode :passthrough))
                (second (set-current-session manager session-id :mode :passthrough)))
            (check (and first second)
                   "The session does not accept two attachments.")
            (set-input-draft first
                             (format nil "printf 'broadcast-marker\\n'; sleep 1~%"))
            (check (set-input-submission first)
                   "The attached frontend cannot submit input.")
            (check (and (attachment-output-has-text-p first
                                                      "broadcast-marker")
                        (attachment-output-has-text-p second
                                                      "broadcast-marker"))
                   "The session does not broadcast output to every attachment.")
            (check (and (get-attachment-attached-p first)
                        (get-attachment-attached-p second))
                   "A healthy attachment disconnects unexpectedly.")))
      (del-session-manager manager))))

(deftest attachments-keep-private-input-drafts ()
  (let ((manager (new-session-manager :retention-seconds 5)))
    (unwind-protect
        (let* ((session-id (new-session manager
                                          :shell "/bin/sh"
                                          :mode :passthrough))
               (first (set-current-session manager session-id :mode :passthrough))
               (second (set-current-session manager session-id :mode :passthrough)))
          (set-input-draft first "printf 'draft-marker\\n'")
          (set-input-draft second "printf 'other-draft\\n'")
          (check (and (string= "printf 'draft-marker\\n'"
                               (get-input-draft first))
                      (string= "printf 'other-draft\\n'"
                               (get-input-draft second)))
                 "Attachments do not keep private input drafts.")
          (check (set-input-submission first)
                 "The draft submission is rejected.")
          (check (string= "" (get-input-draft first))
                 "A submitted draft remains in the frontend input.")
          (check (string= "printf 'other-draft\\n'"
                          (get-input-draft second))
                 "One attachment changes another draft.")
          (check (del-current-session second)
                 "The second attachment cannot del-current-session.")
          (check (not (set-input-submission second))
                 "A detached frontend submits input.")
          (check (string= "printf 'other-draft\\n'"
                          (get-input-draft second))
                 "A rejected draft does not stay in the frontend input."))
      (del-session-manager manager))))

(deftest slow-attachment-disconnects-after-buffer-overflow ()
  (let ((manager (new-session-manager
                  :retention-seconds 5
                  :max-buffer-bytes 8192)))
    (unwind-protect
        (let* ((session-id (new-session manager
                                          :shell "/bin/sh"
                                          :mode :passthrough))
               (slow (set-current-session manager session-id :mode :passthrough))
               (healthy (set-current-session manager session-id :mode :passthrough)))
          (set-input-draft healthy
                           (format nil
                                   "i=0; while [ $i -lt 100 ]; do printf 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'; i=$((i+1)); done; sleep 1~%"))
          (check (set-input-submission healthy)
                 "The overflow test input is rejected.")
          (check (wait-until
                  (lambda ()
                    (get-attachment-output healthy :wait-p nil)
                    (not (get-attachment-attached-p slow)))
                  :attempts 300
                  :delay 0.01)
                 "A slow attachment survives buffer overflow.")
          (check (get-attachment-attached-p healthy)
                 "A healthy attachment disconnects with the slow one."))
      (del-session-manager manager))))

(deftest blocked-attachment-reader-wakes-after-buffer-overflow ()
  (let ((manager (new-session-manager
                  :retention-seconds 5
                  :max-buffer-bytes 32)))
    (unwind-protect
        (let* ((session-id (new-session manager
                                          :shell "/bin/sh"
                                          :mode :passthrough))
               (slow (set-current-session manager session-id :mode :passthrough))
               (writer (set-current-session manager session-id :mode :passthrough))
               (result nil)
               (reader nil)
               (ready-lock (make-lock "reader readiness"))
               (ready-condition (make-condition-variable :name "reader readiness"))
               (ready-p nil))
          (drain-attachment slow)
          (setf reader
                (make-thread
                 (lambda ()
                   (with-lock-held (ready-lock)
                     (setf ready-p t)
                     (condition-notify ready-condition))
                   (loop
                     (multiple-value-bind (bytes eof-p)
                         (get-attachment-output slow)
                       (declare (ignore bytes))
                       (when eof-p
                         (setf result (list nil t))
                         (return)))))))
          (with-lock-held (ready-lock)
            (loop until ready-p
                  do (condition-wait ready-condition ready-lock)))
          (set-input-draft writer
                           (format nil "printf 'overflow-overflow-overflow-overflow'; sleep 1~%"))
          (check (set-input-submission writer)
                 "The overflow wakeup input is rejected.")
          (check (wait-until (lambda () (not (get-attachment-attached-p slow))))
                 "Buffer overflow does not disconnect the slow attachment.")
          (join-thread reader)
          (check (and result (null (first result)) (second result))
                 "A blocked reader does not wake after disconnection."))
      (del-session-manager manager))))

(deftest terminated-session-retains-screen-for-a-fixed-time ()
  (let ((manager (new-session-manager :retention-seconds 0.2)))
    (unwind-protect
        (let* ((session-id (new-session manager
                                          :shell "/bin/sh"
                                          :mode :passthrough))
               (session (get-session manager session-id))
               (attachment (set-current-session manager
                                            session-id
                                            :mode :passthrough)))
          (set-input-draft attachment
                           (format nil "printf 'final-marker\\n'; exit~%"))
          (check (set-input-submission attachment)
                 "The final-screen input is rejected.")
          (check (wait-until (lambda () (not (get-session-running-p session))))
                 "The session does not terminate naturally.")
          (check (get-session manager session-id)
                 "The manager drops the retained session too early.")
          (check (screen-has-text-p (get-retained-screen session) "final-marker")
                 "The retained screen loses final output.")
                 (check (wait-until (lambda () (null (get-session manager session-id)))
                             :attempts 100
                             :delay 0.01)
                 "The manager keeps final screen data beyond its retention time.")
          (check (null (get-attachment-screen attachment))
                 "An attachment keeps final screen data beyond retention."))
      (del-session-manager manager))))

(deftest explicit-termination-stops-managed-session ()
  (let ((manager (new-session-manager :retention-seconds 5)))
    (unwind-protect
        (let* ((session-id (new-session manager :shell "/bin/sh"))
               (session (get-session manager session-id))
               (attachment (set-current-session manager session-id)))
          (check (del-session manager session-id)
                 "Explicit termination does not return success.")
          (check (not (get-session-running-p session))
                 "Explicit termination leaves the session running.")
          (check (not (get-attachment-attached-p attachment))
                 "Explicit termination leaves the attachment connected.")
          (check (null (set-current-session manager session-id))
                 "Explicit termination allows restoration."))
      (del-session-manager manager))))

(deftest passthrough-frontend-works-from-an-attachment ()
  (let ((manager (new-session-manager :retention-seconds 5)))
    (unwind-protect
        (let* ((session-id (new-session manager
                                          :shell "/bin/sh"
                                          :mode :passthrough))
               (session (get-session manager session-id))
               (attachment (set-current-session manager session-id :mode :passthrough))
               (finished-p nil)
               (thread
                 (make-thread
                  (lambda ()
                    (set-passthrough-frontend :attachment attachment
                                     :input-fd nil
                                     :output-fd nil)
                    (setf finished-p t)))))
          (set-input-draft attachment
                           (format nil "printf 'passthrough-marker\\n'; sleep 1~%"))
          (check (set-input-submission attachment)
                 "The passthrough frontend cannot submit input.")
          (check (wait-until
                  (lambda ()
                    (screen-has-text-p (get-attachment-screen attachment)
                                       "passthrough-marker")))
                 "The passthrough attachment does not receive output.")
          (check (del-current-session attachment)
                 "The passthrough frontend cannot del-current-session.")
          (join-thread thread)
          (check finished-p
                 "The passthrough frontend does not stop after del-current-session.")
          (check (get-session-running-p session)
                 "Passthrough detachment terminates the shell session."))
      (del-session-manager manager))))

(deftest attached-passthrough-redraws-status-line-after-reset ()
  (let ((manager (new-session-manager :retention-seconds 5)))
    (multiple-value-bind (read-pipe write-pipe)
        (make-test-pipe)
      (unwind-protect
          (let* ((session-id (new-session manager
                                            :shell "/bin/sh"
                                            :width 20
                                            :height 4
                                            :mode :passthrough))
                 (attachment (set-current-session manager
                                             session-id
                                             :mode :passthrough))
                 (captured "")
                 (thread
                   (make-thread
                    (lambda ()
                      (set-passthrough-frontend :attachment attachment
                                       :input-fd nil
                                       :output-fd write-pipe)))))
            (set-input-draft attachment
                             (format nil
                                     "printf '\\033c'; printf 'status-marker\\n'; sleep 1~%"))
            (check (set-input-submission attachment)
                   "The reset test input is rejected.")
            (check
             (wait-until
              (lambda ()
                (setf captured
                      (concatenate 'string
                                   captured
                                   (drain-test-pipe read-pipe)))
                (and
                 (let* ((reset-position
                          (search (format nil "~Cc" #\Escape) captured))
                        (status-position
                          (and reset-position
                               (search (format nil "~C[30;42m" #\Escape)
                                       captured
                                       :start2 reset-position))))
                   (and (search "status-marker" captured)
                        reset-position
                        status-position
                        (> status-position reset-position)))
                 (get-attachment-attached-p attachment))))
             "Attached output removes the status line.")
            (check (del-current-session attachment)
                   "The reset test attachment cannot del-current-session.")
            (join-thread thread))
        (del-pty read-pipe)
        (del-pty write-pipe)
        (del-session-manager manager)))))

(deftest terminal-writes-text ()
  (let ((terminal (new-terminal-emulator :width 8 :height 2)))
    (set-terminal-input terminal "hello")
    (check (string= "hello   " (first (get-terminal-screen-lines terminal)))
           "The first screen line does not contain the text.")))

(deftest utf8-round-trips-unicode ()
  (let ((bytes (get-utf8 "hé界")))
    (check (equalp #(104 195 169 231 149 140) bytes)
           "UTF-8 encoding has unexpected bytes.")
    (multiple-value-bind (text pending)
        (get-utf8-chunk bytes)
      (check (string= "hé界" text)
             "UTF-8 decoding has unexpected text.")
      (check (null pending)
             "UTF-8 decoding leaves unexpected pending bytes."))))

(deftest utf8-decoder-keeps-split-character ()
  (let ((bytes (get-utf8 "界")))
    (multiple-value-bind (first pending)
        (get-utf8-chunk (subseq bytes 0 2))
      (check (string= "" first)
             "UTF-8 decoding emits an incomplete character.")
      (multiple-value-bind (second remaining)
          (get-utf8-chunk (subseq bytes 2) pending)
        (check (string= "界" second)
               "UTF-8 decoding loses a split character.")
        (check (null remaining)
               "UTF-8 decoding keeps complete character bytes.")))))

(deftest pty-session-runs-a-shell ()
  (let ((session (new-shell-session :shell "/bin/sh" :width 80 :height 24)))
    (unwind-protect
        (progn
          (check (session-open-p session)
                 "The PTY session does not start open.")
          (check (integerp (lispore:pty-master session))
                 "The PTY master accessor does not return a descriptor.")
          (set-shell-size session 100 40)
          (set-shell-input session
                       (format nil "stty size; printf 'lispore-marker\\n'; exit~%"))
          (let ((output (with-output-to-string (stream)
                          (loop for chunk = (get-shell-output session)
                                while chunk
                                do (write-string chunk stream)))))
            (check (search "40 100" output)
                   "The shell does not observe the resized PTY.")
            (check (search "lispore-marker" output)
                   "The shell output does not contain the marker.")))
      (del-shell-session session))
    (check (not (session-open-p session))
           "The PTY session remains open after close.")))

(deftest terminal-applies-ansi-cursor-and-erase ()
  (let ((terminal (new-terminal-emulator :width 8 :height 2)))
    (set-terminal-input terminal (format nil "abc~C[2J~C[Hxy" #\Escape #\Escape))
    (check (string= "xy      " (first (get-terminal-screen-lines terminal)))
           "ANSI cursor or erase handling is incorrect.")
    (multiple-value-bind (row column)
        (get-terminal-cursor-position terminal)
      (check (and (= row 1) (= column 3))
             "The cursor position is incorrect."))))

(deftest terminal-keeps-csi-device-query-quiet ()
  (let ((terminal (new-terminal-emulator :width 8 :height 2)))
    (set-terminal-input terminal (format nil "abc~C[cx" #\Escape))
    (check (string= "abcx    " (first (get-terminal-screen-lines terminal)))
           "CSI device attributes reset the terminal unexpectedly.")))

(deftest terminal-exits-osc-at-ascii-bell ()
  (let* ((terminal (new-terminal-emulator :width 20 :height 3))
         (escape (string #\Escape))
         ;; ASCII BEL ends an OSC sequence.
         (text (concatenate 'string
                            escape "]0;title"
                            (string (code-char 7))
                            "prompt")))
    (set-terminal-input terminal text)
    (check (search "prompt" (first (get-terminal-screen-lines terminal)))
           "OSC does not end at ASCII BEL.")))

(deftest terminal-erases-from-cursor-to-line-end ()
  (let ((terminal (new-terminal-emulator :width 8 :height 2)))
    (set-terminal-input terminal (format nil "abcdef~C[3G~C[K" #\Escape #\Escape))
    (check (string= "ab      " (first (get-terminal-screen-lines terminal)))
           "CSI del-terminal-line does not erase through the line end.")
    (let ((second-terminal (new-terminal-emulator :width 8 :height 2)))
      (set-terminal-input second-terminal
                     (format nil "abcdef~C[3G~C[1K" #\Escape #\Escape))
      (check (string= "   def  " (first (get-terminal-screen-lines second-terminal)))
             "CSI del-terminal-line does not erase through the cursor."))))

(deftest terminal-renders-independent-lines ()
  (let ((terminal (new-terminal-emulator :width 3 :height 2)))
    (set-terminal-input terminal (format nil "a~C~Cb" #\Return #\Newline))
    (check (search (format nil "a  ~C~Cb" #\Return #\Newline)
                   (get-terminal-render terminal))
           "Rendered rows do not start at the first column.")))

(deftest terminal-keeps-sgr-style-on-screen-cells ()
  (let ((terminal (new-terminal-emulator :width 8 :height 2)))
    (set-terminal-input terminal (format nil "~C[31mred~C[0mplain"
                                    #\Escape #\Escape))
    (let ((styled (get-terminal-cell terminal 1 1))
          (plain (get-terminal-cell terminal 1 4)))
      (check (and (char= #\r (screen-cell-character styled))
                  (member 31 (screen-cell-style styled)))
             "The terminal does not store SGR style.")
      (check (null (screen-cell-style plain))
             "The terminal does not clear SGR style."))))

(deftest terminal-reserves-a-colored-status-line ()
  (let ((terminal (new-terminal-emulator :width 20 :height 4)))
    (set-terminal-status-line terminal " lispore | shell ")
    (set-terminal-input terminal (format nil "content~C[4;1Hbottom"
                                    #\Escape))
    (let ((status (get-terminal-cell terminal 4 1)))
      (check (string= " lispore | shell    "
                      (fourth (get-terminal-screen-lines terminal)))
             "The terminal does not render the status line.")
      (check (and (member 30 (screen-cell-style status))
                  (member 42 (screen-cell-style status)))
             "The status line does not use black text on green."))
    (check (search "bottom" (third (get-terminal-screen-lines terminal)))
           "Shell output overwrites the status line.")))

(deftest terminal-preserves-status-line-after-resize-and-reset ()
  (let ((terminal (new-terminal-emulator :width 20 :height 4)))
    (set-terminal-status-line terminal " lispore | shell ")
    (set-terminal-size terminal 12 3)
    (set-terminal-input terminal (format nil "~Cc" #\Escape))
    (check (string= " lispore | s" (third (get-terminal-screen-lines terminal)))
           "The status line does not survive terminal reset.")
    (check (and (= 12 (length (third (get-terminal-screen-lines terminal))))
                (some (lambda (line) (search " lispore | " line))
                      (get-terminal-screen-lines terminal)))
           "The resized status line has an unexpected width.")))

(deftest passthrough-status-line-uses-fixed-ansi-layout ()
  (let ((output (lispore.frontend::get-passthrough-status-line 20 4)))
    (check (search (format nil "~C[1;3r" #\Escape) output)
           "Passthrough mode does not reserve one row.")
    (check (search (format nil "~C[30;42m" #\Escape) output)
           "Passthrough mode does not set status colors.")
    (check (search " lispore | shell " output)
           "Passthrough mode does not render status text.")))

(deftest passthrough-frontend-drains-a-session ()
  (let ((session (new-shell-session :shell "/bin/sh" :width 80 :height 24)))
    (unwind-protect
        (progn
          (set-shell-input session (format nil "printf 'passthrough-marker\\n'; exit~%"))
          (check (integerp (set-passthrough-frontend :session session
                                             :input-fd nil
                                             :output-fd nil))
                 "The passthrough frontend has no exit status."))
      (del-shell-session session))))

(defun terminal-settings (fd)
  "Return terminal settings for FD as text."
  (uiop:run-program (list "stty" "-g")
                    :input (format nil "/dev/fd/~D" fd)
                    :output :string))

(deftest raw-terminal-restores-after-normal-exit-and-error ()
  (let ((session (new-shell-session :shell "/bin/sh" :width 80 :height 24)))
    (unwind-protect
        (progn
          (let ((fd (lispore:pty-master session)))
            (check (get-tty-p fd)
                 "The PTY master is not a terminal descriptor.")
            (let ((before (terminal-settings fd)))
              (set-raw-terminal (lambda () (values)) :fd fd)
              (check (string= before (terminal-settings fd))
                     "Normal raw-terminal exit does not restore settings.")
              (let ((raised nil))
                (handler-case
                    (set-raw-terminal
                     (lambda () (error "expected raw-terminal error"))
                     :fd fd)
                  (error () (setf raised t)))
                (check raised
                       "The raw-terminal body does not propagate errors.")
                (check (string= before (terminal-settings fd))
                       "Error raw-terminal exit does not restore settings.")))))
      (del-shell-session session))))

(defun set-tests ()
  (let ((passed 0)
        (failed 0))
    (dolist (test (reverse *tests*))
      (handler-case
          (progn
            (funcall test)
            (incf passed)
            (format t "PASS ~A~%" test))
        (error (condition)
          (incf failed)
          (format t "FAIL ~A: ~A~%" test condition))))
    (format t "~A passed, ~A failed.~%" passed failed)
    (when (plusp failed)
      (error "The test suite has failures."))
    t))
