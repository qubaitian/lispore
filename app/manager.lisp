(in-package #:lispore.app)

(defparameter +manager-server-argument+ "--manager-server")
(defparameter +manager-connect-attempts+ 100)
(defparameter +manager-connect-delay+ 0.05)
(defparameter +manager-socket-name+ "manager.sock")
(defparameter +manager-debug-log-name+ "debug.log")

(defun get-manager-socket-path ()
  "Return the per-user manager socket path."
  (merge-pathnames
   (format nil ".lispore/~A" +manager-socket-name+)
   (user-homedir-pathname)))

(defun get-manager-debug-log-path ()
  "Return the per-user diagnostic log path."
  (merge-pathnames
   (format nil ".lispore/~A" +manager-debug-log-name+)
   (user-homedir-pathname)))

(defun get-manager-socket-fd (socket)
  "Return SOCKET's file descriptor on SBCL."
  #+sbcl
  (sb-bsd-sockets:socket-file-descriptor (usocket:socket socket))
  #-sbcl
  (declare (ignore socket))
  #-sbcl
  (error "The Lispore manager requires SBCL."))

(defun set-protocol-line (fd line)
  "Write one ASCII protocol line to FD."
  (set-fd fd (get-utf8 (format nil "~A~%" line))))

(defun get-protocol-line (fd)
  "Read one ASCII protocol line from FD."
  (with-output-to-string (output)
    (loop
      (multiple-value-bind (bytes eof-p)
          (get-fd fd :max-bytes 1 :wait-p t)
        (when eof-p
          (return-from get-protocol-line nil))
        (let ((byte (aref bytes 0)))
          (cond
            ((= byte 10)
             (return))
            ((/= byte 13)
             (write-char (code-char byte) output))))))))

(defun del-manager-socket (socket)
  "Close SOCKET without masking an earlier condition."
  (when socket
    (ignore-errors (usocket:socket-close socket))))

(defun new-manager-connection ()
  "Connect to the running manager, or return NIL."
  (handler-case
      (let ((socket
              (usocket:socket-connect
               (get-manager-socket-path)
               nil
               :element-type '(unsigned-byte 8))))
        (handler-case
            (progn
              (set-socket-sigpipe (get-manager-socket-fd socket))
              (set-close-on-exec (get-manager-socket-fd socket))
              socket)
          (error (condition)
            (del-manager-socket socket)
            (error condition))))
    (error () nil)))

(defun manager-responding-p ()
  "Return true when the manager socket accepts a protocol request."
  (handler-case
      (let ((socket (new-manager-connection)))
        (when socket
          (unwind-protect
               (let ((fd (get-manager-socket-fd socket)))
                 (set-protocol-line fd "PING")
                 (string= "PONG" (get-protocol-line fd)))
            (del-manager-socket socket))))
    (error () nil)))

(defun get-manager-program ()
  "Return the standalone executable used to start the manager."
  (or (uiop:argv0)
      (error "The manager requires the standalone lispore executable.")))

(defun new-manager-process ()
  "Start the manager as a detached child process."
  (uiop:launch-program
   (list (get-manager-program) +manager-server-argument+)
   :input nil
   :output nil
   :error-output nil
   :wait nil))

(defun get-manager-ready ()
  "Wait until the manager accepts connections."
  (loop repeat +manager-connect-attempts+
        for socket = (new-manager-connection)
        when socket
          do (return socket)
        do (sleep +manager-connect-delay+)
        finally (error "The lispore session manager did not start.")))

(defun get-or-new-manager-connection ()
  "Return a connection to the running manager."
  (or (new-manager-connection)
      (progn
        (new-manager-process)
        (get-manager-ready))))

(defun get-positive-integer (text)
  "Return positive integer TEXT, or NIL."
  (handler-case
      (let ((value (parse-integer text)))
        (when (plusp value)
          value))
    (error () nil)))

(defun get-integer (text)
  "Return integer TEXT, or NIL."
  (handler-case
      (parse-integer text)
    (error () nil)))

(defun protocol-command-p (parts command count)
  "Return true when PARTS starts with COMMAND and has COUNT items."
  (and (= (length parts) count)
       (string= (first parts) command)))

(defun get-session-state-name (state)
  "Return STATE as lower-case protocol text."
  (string-downcase (symbol-name state)))

(defun set-value-response (fd path value)
  "Write one scalar value response to FD."
  (set-protocol-line fd (format nil "VALUE ~A ~A" path value)))

(defun set-session-list-response (fd manager)
  "Write the manager's named session list to FD."
  (dolist (entry (get-session-list manager))
    (set-protocol-line
     fd
     (format nil "SESSION ~A ~A"
             (car entry)
             (get-session-state-name (cdr entry)))))
  (set-protocol-line fd "END"))

(defun set-manager-log-broadcast (manager record)
  "Publish RECORD to every active named session."
  (dolist (entry (get-session-list manager))
    (let ((session (get-session-by-name manager (car entry))))
      (when session
        (ignore-errors
          (set-session-published-output
           session
           (format nil "~%[lispore debug]~%~A" record)))))))

(defun new-manager-debug-log ()
  "Open the append-only per-user diagnostic log."
  (let ((path (get-manager-debug-log-path)))
    (ensure-directories-exist path)
    (values (open path
                  :direction :output
                  :if-exists :append
                  :if-does-not-exist :create
                  :external-format :utf-8)
            path)))

(defun set-manager-debug (manager value)
  "Set manager diagnostics to VALUE and return VALUE."
  (unless (member value '(0 1) :test #'eql)
    (error "Debug value must be 0 or 1."))
  (if (zerop value)
      (del-session-manager-logger manager)
      (unless (get-manager-debug-enabled-p manager)
        (multiple-value-bind (stream log-path)
            (new-manager-debug-log)
          (declare (ignore log-path))
          (let ((logger
                  (new-diagnostic-logger
                   stream
                   (lambda (record)
                     (set-manager-log-broadcast manager record)))))
            (let ((installed (set-session-manager-logger manager logger)))
              (unless (eq installed logger)
                (del-diagnostic-logger logger)))))))
  (set-manager-debug-value manager value)
  (when (= value 1)
    (set-manager-log manager
                     "debug-enabled"
                     :message (namestring (get-manager-debug-log-path))))
  value)

(defun new-session-request (fd parts manager)
  "Create the named Session requested by PARTS."
  (unless (protocol-command-p parts "NEW" 3)
    (error "The manager received an invalid NEW request."))
  (unless (string= (second parts) "SESSION")
    (error "NEW supports only the SESSION position."))
  (let* ((name (third parts))
         (session-id (new-session manager :name name :mode :command))
         (session (get-session manager session-id)))
    (unless session
      (error "The new Session is not registered."))
    (set-value-response fd "SESSION" (session-name session))))

(defun get-session-request (fd parts manager)
  "Return the value requested by a GET operation."
  (unless (protocol-command-p parts "GET" 2)
    (error "The manager received an invalid GET request."))
  (let ((path (second parts)))
    (cond
      ((string= path "SESSION")
       (set-session-list-response fd manager))
      ((string= path "DEBUG")
       (set-value-response fd "DEBUG" (get-manager-debug-value manager)))
      ((string= path "CURRENT-SESSION")
       (error "The CLI current-session position is local to one process."))
      (t
       (error "GET does not support position ~A." path)))))

(defun set-debug-request (fd parts manager)
  "Set the Debug value requested by PARTS."
  (unless (protocol-command-p parts "SET" 3)
    (error "The manager received an invalid SET request."))
  (unless (string= (second parts) "DEBUG")
    (error "SET does not support position ~A." (second parts)))
  (let ((value (get-integer (third parts))))
    (unless value
      (error "SET DEBUG requires an integer value."))
    (set-value-response fd "DEBUG" (set-manager-debug manager value))))

(defun set-current-session-request (fd parts manager)
  "Enter the existing Session requested by PARTS."
  (unless (protocol-command-p parts "SET" 5)
    (error "The manager received an invalid SET CURRENT-SESSION request."))
  (unless (string= (second parts) "CURRENT-SESSION")
    (error "SET does not support position ~A." (second parts)))
  (let ((name (third parts))
        (width (get-positive-integer (fourth parts)))
        (height (get-positive-integer (fifth parts))))
    (unless (and width height)
      (error "The manager received invalid terminal dimensions."))
    (let ((session (get-session-by-name manager name)))
      (unless session
        (error "The Session named ~A does not exist." name))
      (let ((attachment (set-current-session manager
                                             (session-id session)
                                             :mode :command)))
        (unless attachment
          (error "The Session named ~A cannot accept an attachment." name))
        (handler-case
            (progn
              (set-protocol-line fd "READY")
              ;; The frontend speaks directly through the socket descriptor.
              (set-interactive-shell :attachment attachment
                                     :input-fd fd
                                     :output-fd fd))
          (error (condition)
            ;; A failed handoff must not leave a stale attachment.
            (ignore-errors (del-current-session attachment))
            (error condition)))))))

(defun del-session-request (fd parts manager)
  "Delete the named Session requested by PARTS."
  (unless (protocol-command-p parts "DEL" 3)
    (error "The manager received an invalid DEL request."))
  (unless (string= (second parts) "SESSION")
    (error "DEL cannot remove position ~A." (second parts)))
  (let* ((name (third parts))
         (session (get-session-by-name manager name)))
    (unless session
      (error "The Session named ~A does not exist." name))
    (unless (del-session manager (session-id session))
      (error "The Session named ~A cannot be deleted." name))
    (set-value-response fd "SESSION" name)))

(defun set-manager-error-response (fd manager line condition)
  "Log CONDITION and write its error response to FD."
  (ignore-errors
    (set-manager-log manager
                     "manager-error"
                     :message line
                     :condition condition))
  (ignore-errors
    (set-protocol-line fd (format nil "ERROR ~A" condition))))

(defun set-manager-client (socket manager)
  "Handle one manager protocol connection."
  (let ((fd (get-manager-socket-fd socket)))
    (set-socket-sigpipe fd)
    (unwind-protect
         (let ((line (get-protocol-line fd)))
           (when line
             (let ((parts (uiop:split-string line)))
               (cond
                 ((protocol-command-p parts "PING" 1)
                  (set-manager-log manager "manager-request" :message line)
                  (set-protocol-line fd "PONG"))
                 ((and (protocol-command-p parts "NEW" 3)
                       (string= (second parts) "SESSION"))
                  (handler-case
                      (progn
                        (set-manager-log manager "manager-request" :message line)
                        (new-session-request fd parts manager))
                    (error (condition)
                      (set-manager-error-response fd manager line condition))))
                 ((protocol-command-p parts "GET" 2)
                  (handler-case
                      (progn
                        (set-manager-log manager "manager-request" :message line)
                        (get-session-request fd parts manager))
                    (error (condition)
                      (set-manager-error-response fd manager line condition))))
                 ((and (protocol-command-p parts "SET" 3)
                       (string= (second parts) "DEBUG"))
                  (handler-case
                      (progn
                        (set-manager-log manager "manager-request" :message line)
                        (set-debug-request fd parts manager))
                    (error (condition)
                      (set-manager-error-response fd manager line condition))))
                 ((and (protocol-command-p parts "SET" 5)
                       (string= (second parts) "CURRENT-SESSION"))
                  (handler-case
                      (progn
                        (set-manager-log manager "manager-request" :message line)
                        (set-current-session-request fd parts manager))
                    (error (condition)
                      (set-manager-error-response fd manager line condition))))
                 ((and (protocol-command-p parts "DEL" 3)
                       (string= (second parts) "SESSION"))
                  (handler-case
                      (progn
                        (set-manager-log manager "manager-request" :message line)
                        (del-session-request fd parts manager))
                    (error (condition)
                      (set-manager-error-response fd manager line condition))))
                 ((protocol-command-p parts "NEW" (length parts))
                  (set-manager-error-response
                   fd manager line
                   (make-condition 'simple-error
                                   :format-control
                                   "NEW does not support position ~A."
                                   :format-arguments (list (second parts)))))
                 ((protocol-command-p parts "DEL" (length parts))
                  (set-manager-error-response
                   fd manager line
                   (make-condition 'simple-error
                                   :format-control
                                   "DEL cannot remove position ~A."
                                   :format-arguments (list (second parts)))))
                 (t
                  (set-manager-error-response
                   fd manager line
                   (make-condition 'simple-error
                                   :format-control "Invalid manager request."
                                   :format-arguments nil)))))))
      (del-manager-socket socket))))

(defun new-manager-listener ()
  "Start the manager listener, or return NIL for a live manager."
  (let ((path (get-manager-socket-path)))
    (ensure-directories-exist path)
    (loop repeat +manager-connect-attempts+
          do (handler-case
                 (return
                   (let ((listener
                           (usocket:socket-listen
                            path
                            nil
                            :element-type '(unsigned-byte 8)
                            :backlog 16)))
                     (handler-case
                         (progn
                           (set-close-on-exec (get-manager-socket-fd listener))
                           listener)
                       (error (condition)
                         (del-manager-socket listener)
                         (error condition)))))
               (error ()
                 (when (manager-responding-p)
                   (return nil))
                 (ignore-errors (delete-file path))
                 (sleep +manager-connect-delay+)))
          finally (error "The manager socket cannot be opened."))))

(defun set-manager-accept-loop (listener manager)
  "Accept manager protocol connections forever."
  (loop
    for socket = (usocket:socket-accept listener)
    when socket
      do (let ((client socket))
           (handler-case
               (progn
                 (set-close-on-exec (get-manager-socket-fd client))
                 (make-thread
                  (lambda ()
                    (handler-case
                        (set-manager-client client manager)
                      (error (condition)
                        (ignore-errors
                          (set-manager-log manager
                                           "manager-client-error"
                                           :condition condition))
                        (del-manager-socket client))))
                  :name "lispore manager client"))
             (error (condition)
               (ignore-errors
                 (set-manager-log manager
                                  "manager-client-error"
                                  :condition condition))
               (del-manager-socket client))))))

(defun set-manager-server ()
  "Run the background session manager process."
  (let ((listener (new-manager-listener)))
    (when listener
      (let ((manager (new-session-manager)))
        (unwind-protect
             (handler-case
                 (set-manager-accept-loop listener manager)
               (error (condition)
                 (ignore-errors
                   (set-manager-log manager
                                    "manager-server-error"
                                    :condition condition))
                 (error condition)))
          (del-manager-socket listener)
          (ignore-errors (delete-file (get-manager-socket-path)))
          (del-session-manager manager))))))

(defun get-client-terminal-dimensions ()
  "Return the client terminal size with safe defaults."
  (multiple-value-bind (width height)
      (get-terminal-size 0)
    (values (if (and width (plusp width)) width 80)
            (if (and height (plusp height)) height 24))))

(defun get-cli-value-response (socket path)
  "Read and validate one scalar value response from SOCKET."
  (let ((line (get-protocol-line (get-manager-socket-fd socket))))
    (cond
      ((null line)
       (error "The manager closed the response connection."))
      ((uiop:string-prefix-p "ERROR " line)
       (error "~A" (subseq line (length "ERROR "))))
      (t
       (let ((parts (uiop:split-string line)))
         (unless (and (= (length parts) 3)
                      (string= (first parts) "VALUE")
                      (string= (second parts) path))
           (error "The manager returned an invalid value."))
         (third parts))))))

(defun new-cli-session (name)
  "Create NAME through the manager and print its value."
  (let* ((socket (get-or-new-manager-connection))
         (fd (get-manager-socket-fd socket)))
    (unwind-protect
         (progn
           (set-protocol-line fd (format nil "NEW SESSION ~A" name))
           (format t "~A~%" (get-cli-value-response socket "SESSION")))
      (del-manager-socket socket))))

(defun get-cli-session-list ()
  "Print the manager's named Session list."
  (let* ((socket (get-or-new-manager-connection))
         (fd (get-manager-socket-fd socket))
         (count 0))
    (unwind-protect
         (progn
           (set-protocol-line fd "GET SESSION")
           (loop for line = (get-protocol-line fd)
                 do (when (null line)
                      (error "The manager closed the Session list connection."))
                    (cond
                      ((string= line "END")
                       (return))
                      ((uiop:string-prefix-p "SESSION " line)
                       (let ((parts (uiop:split-string line)))
                         (unless (= (length parts) 3)
                           (error "The manager returned an invalid Session."))
                         (incf count)
                         (format t "~A~C~A~%"
                                 (second parts)
                                 #\Tab
                                 (third parts))))
                      ((uiop:string-prefix-p "ERROR " line)
                       (error "~A" (subseq line (length "ERROR "))))
                      (t
                       (error "The manager returned an invalid Session list."))))
           (when (zerop count)
             (format t "No sessions.~%")))
      (del-manager-socket socket))))

(defun get-cli-debug ()
  "Print the manager Debug value."
  (let* ((socket (get-or-new-manager-connection))
         (fd (get-manager-socket-fd socket)))
    (unwind-protect
         (progn
           (set-protocol-line fd "GET DEBUG")
           (format t "~A~%" (get-cli-value-response socket "DEBUG")))
      (del-manager-socket socket))))

(defun set-cli-debug (value)
  "Set and print the manager Debug value."
  (let* ((socket (get-or-new-manager-connection))
         (fd (get-manager-socket-fd socket)))
    (unwind-protect
         (progn
           (set-protocol-line fd (format nil "SET DEBUG ~A" value))
           (format t "~A~%" (get-cli-value-response socket "DEBUG")))
      (del-manager-socket socket))))

(defun set-cli-current-session (name)
  "Enter NAME and return its manager connection."
  (multiple-value-bind (width height)
      (get-client-terminal-dimensions)
    (let* ((socket (get-or-new-manager-connection))
           (fd (get-manager-socket-fd socket)))
      (handler-case
          (progn
            (set-protocol-line
             fd
             (format nil "SET CURRENT-SESSION ~A ~D ~D"
                     name width height))
            (let ((response (get-protocol-line fd)))
              (cond
                ((and response (string= response "READY"))
                 socket)
                ((and response (uiop:string-prefix-p "ERROR " response))
                 (error "~A" (subseq response (length "ERROR "))))
                (t
                 (error "The manager rejected the current-session request.")))))
        (error (condition)
          (del-manager-socket socket)
          (error condition))))))

(defun set-cli-current-session-frontend (name)
  "Enter NAME through the local terminal."
  (let ((socket (set-cli-current-session name)))
    (unwind-protect
         (set-client-frontend socket)
      (del-manager-socket socket))))

(defun del-cli-session (name)
  "Delete NAME through the manager and print its value."
  (let* ((socket (get-or-new-manager-connection))
         (fd (get-manager-socket-fd socket)))
    (unwind-protect
         (progn
           (set-protocol-line fd (format nil "DEL SESSION ~A" name))
           (format t "~A~%" (get-cli-value-response socket "SESSION")))
      (del-manager-socket socket))))

(defun set-client-frontend (socket)
  "Forward the local terminal to SOCKET."
  (let ((socket-fd (get-manager-socket-fd socket))
        (finished-p nil))
    (set-socket-sigpipe socket-fd)
    (set-raw-terminal
     (lambda ()
       (loop until finished-p
             for events = (get-poll-events (list (cons 0 +pollin+)
                                                 (cons socket-fd +pollin+))
                                           :timeout -1)
             do (when (get-socket-event-readable-p events socket-fd)
                  (multiple-value-bind (bytes eof-p)
                      (get-fd socket-fd :wait-p nil)
                    (when (and bytes (plusp (length bytes)))
                      (set-fd 1 bytes))
                    (when eof-p
                      (setf finished-p t))))
                (when (and (not finished-p)
                           (get-socket-event-readable-p events 0))
                  (multiple-value-bind (bytes eof-p)
                      (get-fd 0 :wait-p nil)
                    (if eof-p
                        (setf finished-p t)
                        (when (and bytes (plusp (length bytes)))
                          (set-fd socket-fd bytes)))))))
     :fd 0)))

(defun get-socket-event-readable-p (events fd)
  "Return true when FD has readable or terminal events."
  (let ((revents (cdr (assoc fd events))))
    (and revents
         (plusp (logand revents
                        (logior +pollin+
                                +pollerr+
                                +pollhup+
                                +pollnval+))))))
