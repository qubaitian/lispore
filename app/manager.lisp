(in-package #:lispore.app)

(defparameter +manager-server-argument+ "--manager-server")
(defparameter +manager-connect-attempts+ 100)
(defparameter +manager-connect-delay+ 0.05)
(defparameter +manager-socket-name+ "manager.sock")
(defparameter +manager-debug-command+ "DEBUG")
(defparameter +manager-debug-log-name+ "debug.log")

(defun manager-socket-path ()
  "Return the per-user manager socket path."
  (merge-pathnames
   (format nil ".lispore/~A" +manager-socket-name+)
   (user-homedir-pathname)))

(defun manager-debug-log-path ()
  "Return the per-user diagnostic log path."
  (merge-pathnames
   (format nil ".lispore/~A" +manager-debug-log-name+)
   (user-homedir-pathname)))

(defun manager-socket-fd (socket)
  "Return SOCKET's file descriptor on SBCL."
  #+sbcl
  (sb-bsd-sockets:socket-file-descriptor (usocket:socket socket))
  #-sbcl
  (declare (ignore socket))
  #-sbcl
  (error "The Lispore manager requires SBCL."))

(defun write-protocol-line (fd line)
  "Write one ASCII protocol line to FD."
  (write-fd fd (encode-utf8 (format nil "~A~%" line))))

(defun read-protocol-line (fd)
  "Read one ASCII protocol line from FD."
  (with-output-to-string (output)
    (loop
      (multiple-value-bind (bytes eof-p)
          (read-fd fd :max-bytes 1 :wait-p t)
        (when eof-p
          (return-from read-protocol-line nil))
        (let ((byte (aref bytes 0)))
          (cond
            ((= byte 10)
             (return))
            ((/= byte 13)
             (write-char (code-char byte) output))))))))

(defun close-manager-socket (socket)
  "Close SOCKET without masking an earlier condition."
  (when socket
    (ignore-errors (usocket:socket-close socket))))

(defun connect-to-manager ()
  "Connect to the running manager, or return NIL."
  (handler-case
      (let ((socket
              (usocket:socket-connect
               (manager-socket-path)
               nil
               :element-type '(unsigned-byte 8))))
        (handler-case
            (progn
              (prevent-socket-sigpipe (manager-socket-fd socket))
              (set-close-on-exec (manager-socket-fd socket))
              socket)
          (error (condition)
            (close-manager-socket socket)
            (error condition))))
    (error () nil)))

(defun manager-responding-p ()
  "Return true when the manager socket accepts a protocol request."
  (handler-case
      (let ((socket (connect-to-manager)))
        (when socket
          (unwind-protect
               (let ((fd (manager-socket-fd socket)))
                 (write-protocol-line fd "PING")
                 (string= "PONG" (read-protocol-line fd)))
            (close-manager-socket socket))))
    (error () nil)))

(defun manager-program ()
  "Return the standalone executable used to start the manager."
  (or (uiop:argv0)
      (error "The manager requires the standalone lispore executable.")))

(defun start-manager-process ()
  "Start the manager as a detached child process."
  (uiop:launch-program
   (list (manager-program) +manager-server-argument+)
   :input nil
   :output nil
   :error-output nil
   :wait nil))

(defun wait-for-manager ()
  "Wait until the manager accepts connections."
  (loop repeat +manager-connect-attempts+
        for socket = (connect-to-manager)
        when socket
          do (return socket)
        do (sleep +manager-connect-delay+)
        finally (error "The lispore session manager did not start.")))

(defun ensure-manager-connection ()
  "Return a connection to the running manager."
  (or (connect-to-manager)
      (progn
        (start-manager-process)
        (wait-for-manager))))

(defun parse-positive-integer (text)
  "Return positive integer TEXT, or NIL."
  (handler-case
      (let ((value (parse-integer text)))
        (when (plusp value)
          value))
    (error () nil)))

(defun protocol-command-p (parts command count)
  "Return true when PARTS matches COMMAND and COUNT."
  (and (= (length parts) count)
       (string= (first parts) command)))

(defun session-state-name (state)
  "Return STATE as lower-case protocol text."
  (string-downcase (symbol-name state)))

(defun handle-list-request (fd manager)
  "Write the manager's named session list to FD."
  (dolist (entry (session-list manager))
    (write-protocol-line
     fd
     (format nil "SESSION ~A ~A"
             (car entry)
             (session-state-name (cdr entry)))))
  (write-protocol-line fd "END"))

(defun broadcast-manager-log (manager record)
  "Publish RECORD to every active named session."
  (dolist (entry (session-list manager))
    (let ((session (lookup-session-by-name manager (car entry))))
      (when session
        (ignore-errors
          (publish-session-output
           session
           (format nil "~%[lispore debug]~%~A" record)))))))

(defun open-manager-debug-log ()
  "Open the append-only per-user diagnostic log."
  (let ((path (manager-debug-log-path)))
    (ensure-directories-exist path)
    (values (open path
                  :direction :output
                  :if-exists :append
                  :if-does-not-exist :create
                  :external-format :utf-8)
            path)))

(defun enable-manager-debug (manager)
  "Enable diagnostic logging on MANAGER and return its log path."
  (let ((path (manager-debug-log-path)))
    (unless (manager-debug-enabled-p manager)
      (multiple-value-bind (stream log-path)
          (open-manager-debug-log)
        (let ((logger
                (make-diagnostic-logger
                 stream
                 (lambda (record)
                   (broadcast-manager-log manager record)))))
          (let ((installed (install-session-manager-logger manager logger)))
            (unless (eq installed logger)
              (close-diagnostic-logger logger))))
        (setf path log-path)))
    (manager-log manager
                 "debug-enabled"
                 :message (namestring path))
    path))

(defun handle-debug-request (fd parts manager)
  "Enable manager diagnostics and acknowledge the request."
  (unless (protocol-command-p parts +manager-debug-command+ 1)
    (error "The manager received an invalid DEBUG request."))
  (enable-manager-debug manager)
  (write-protocol-line fd "READY"))

(defun handle-open-request (fd parts manager)
  "Create or attach the requested named session."
  (unless (protocol-command-p parts "OPEN" 4)
    (error "The manager received an invalid OPEN request."))
  (let ((name (second parts))
        (width (parse-positive-integer (third parts)))
        (height (parse-positive-integer (fourth parts))))
    (unless (and width height)
      (error "The manager received invalid terminal dimensions."))
    (let* ((session (find-or-create-session manager
                                            name
                                            :width width
                                            :height height
                                            :mode :command))
           (attachment (attach-session manager (session-id session)
                                       :mode :command)))
      (unless attachment
        (error "The named session cannot accept an attachment."))
      (handler-case
          (progn
            (write-protocol-line fd "READY")
            ;; The existing frontend speaks directly through the socket descriptor.
            (interactive-shell :attachment attachment
                                :input-fd fd
                                :output-fd fd))
        (error (condition)
          ;; A failed handoff must not leave a stale attachment.
          (ignore-errors (detach attachment))
          (error condition))))))

(defun handle-manager-client (socket manager)
  "Handle one manager protocol connection."
  (let ((fd (manager-socket-fd socket)))
    (prevent-socket-sigpipe fd)
    (unwind-protect
         (let ((line (read-protocol-line fd)))
           (when line
             (let ((parts (uiop:split-string line)))
               (cond
                 ((protocol-command-p parts "PING" 1)
                  (manager-log manager "manager-request" :message line)
                  (write-protocol-line fd "PONG"))
                 ((protocol-command-p parts "LIST" 1)
                  (manager-log manager "manager-request" :message line)
                  (handle-list-request fd manager))
                 ((protocol-command-p parts +manager-debug-command+ 1)
                  (handler-case
                      (progn
                        (handle-debug-request fd parts manager)
                        (manager-log manager "manager-request" :message line))
                    (error (condition)
                      (ignore-errors
                        (manager-log manager
                                     "manager-error"
                                     :message line
                                     :condition condition))
                      (ignore-errors
                        (write-protocol-line
                         fd
                         (format nil "ERROR ~A" condition))))))
                 ((protocol-command-p parts "OPEN" 4)
                  (handler-case
                      (progn
                        (manager-log manager "manager-request" :message line)
                        (handle-open-request fd parts manager))
                    (error (condition)
                      (ignore-errors
                        (manager-log manager
                                     "manager-error"
                                     :message line
                                     :condition condition))
                      (ignore-errors
                        (write-protocol-line
                         fd
                         (format nil "ERROR ~A" condition))))))
                 (t
                  (manager-log manager "manager-error" :message line)
                  (write-protocol-line fd "ERROR Invalid manager request.")))))
      (close-manager-socket socket)))))

(defun start-manager-listener ()
  "Start the manager listener, or return NIL for a live manager."
  (let ((path (manager-socket-path)))
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
                           (set-close-on-exec (manager-socket-fd listener))
                           listener)
                       (error (condition)
                         (close-manager-socket listener)
                         (error condition)))))
               (error ()
                 (when (manager-responding-p)
                   (return nil))
                 (ignore-errors (delete-file path))
                 (sleep +manager-connect-delay+)))
          finally (error "The manager socket cannot be opened."))))

(defun run-manager-accept-loop (listener manager)
  "Accept manager protocol connections forever."
  (loop
    for socket = (usocket:socket-accept listener)
    when socket
      do (let ((client socket))
           (handler-case
               (progn
                 (set-close-on-exec (manager-socket-fd client))
                 (make-thread
                  (lambda ()
                    (handler-case
                        (handle-manager-client client manager)
                      (error (condition)
                        (ignore-errors
                          (manager-log manager
                                       "manager-client-error"
                                       :condition condition))
                        (close-manager-socket client))))
                  :name "lispore manager client"))
             (error (condition)
               (ignore-errors
                 (manager-log manager
                              "manager-client-error"
                              :condition condition))
               (close-manager-socket client))))))

(defun run-manager-server ()
  "Run the background session manager process."
  (let ((listener (start-manager-listener)))
    (when listener
      (let ((manager (make-session-manager)))
        (unwind-protect
             (handler-case
                 (run-manager-accept-loop listener manager)
               (error (condition)
                 (ignore-errors
                   (manager-log manager
                                "manager-server-error"
                                :condition condition))
                 (error condition)))
          (close-manager-socket listener)
          (ignore-errors (delete-file (manager-socket-path)))
          (close-session-manager manager))))))

(defun client-terminal-dimensions ()
  "Return the client terminal size with safe defaults."
  (multiple-value-bind (width height)
      (terminal-size 0)
    (values (if (and width (plusp width)) width 80)
            (if (and height (plusp height)) height 24))))

(defun open-named-session (name)
  "Open NAME and return its manager connection."
  (multiple-value-bind (width height)
      (client-terminal-dimensions)
    (let* ((socket (ensure-manager-connection))
           (fd (manager-socket-fd socket)))
      (handler-case
          (progn
            (write-protocol-line
             fd
             (format nil "OPEN ~A ~D ~D" name width height))
            (let ((response (read-protocol-line fd)))
              (cond
                ((and response (string= response "READY"))
                 socket)
                ((and response (uiop:string-prefix-p "ERROR " response))
                 (error "~A" (subseq response (length "ERROR "))))
                (t
                 (error "The manager rejected the session request.")))))
        (error (condition)
          (close-manager-socket socket)
          (error condition))))))

(defun request-manager-debug ()
  "Enable diagnostics on the existing manager and return its log path."
  (let* ((socket (ensure-manager-connection))
         (fd (manager-socket-fd socket)))
    (unwind-protect
         (progn
           (write-protocol-line fd +manager-debug-command+)
           (let ((response (read-protocol-line fd)))
             (cond
               ((and response (string= response "READY"))
                (manager-debug-log-path))
               ((and response (uiop:string-prefix-p "ERROR " response))
                (error "~A" (subseq response (length "ERROR "))))
               (t
                (error "The manager rejected the debug request.")))))
      (close-manager-socket socket))))

(defun socket-event-readable-p (events fd)
  "Return true when FD has readable or terminal events."
  (let ((revents (cdr (assoc fd events))))
    (and revents
         (plusp (logand revents
                        (logior +pollin+
                                +pollerr+
                                +pollhup+
                                +pollnval+))))))

(defun run-client-frontend (socket)
  "Forward the local terminal to SOCKET."
  (let ((socket-fd (manager-socket-fd socket))
        (finished-p nil))
    (prevent-socket-sigpipe socket-fd)
    (call-with-raw-terminal
     (lambda ()
       (loop until finished-p
             for events = (poll-fds (list (cons 0 +pollin+)
                                          (cons socket-fd +pollin+))
                                    :timeout -1)
             do (when (socket-event-readable-p events socket-fd)
                  (multiple-value-bind (bytes eof-p)
                      (read-fd socket-fd :wait-p nil)
                    (when (and bytes (plusp (length bytes)))
                      (write-fd 1 bytes))
                    (when eof-p
                      (setf finished-p t))))
                (when (and (not finished-p)
                           (socket-event-readable-p events 0))
                  (multiple-value-bind (bytes eof-p)
                      (read-fd 0 :wait-p nil)
                    (if eof-p
                        (setf finished-p t)
                        (when (and bytes (plusp (length bytes)))
                          (write-fd socket-fd bytes)))))))
     :fd 0)))

(defun run-named-session (name)
  "Attach the local frontend to NAME."
  (let ((socket (open-named-session name)))
    (unwind-protect
         (run-client-frontend socket)
      (close-manager-socket socket))))

(defun print-session-list ()
  "Print the current named session list."
  (let* ((socket (ensure-manager-connection))
         (fd (manager-socket-fd socket))
         (count 0))
    (unwind-protect
         (progn
           (write-protocol-line fd "LIST")
           (loop for line = (read-protocol-line fd)
                 do (when (null line)
                      (error "The manager closed the list connection."))
                    (cond
                      ((string= line "END")
                       (return))
                      ((uiop:string-prefix-p "SESSION " line)
                       (let ((parts (uiop:split-string line)))
                         (when (= (length parts) 3)
                           (incf count)
                           (format t "~A~C~A~%"
                                   (second parts)
                                   #\Tab
                                   (third parts)))))
                      ((uiop:string-prefix-p "ERROR " line)
                       (error "~A" (subseq line (length "ERROR "))))
                      (t
                       (error "The manager returned an invalid list."))))
           (when (zerop count)
             (format t "No sessions.~%")))
      (close-manager-socket socket))))
