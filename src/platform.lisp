(in-package #:lispore.platform)

(define-foreign-library libc
  (:darwin (:default "libc")))

(use-foreign-library libc)

;; These layouts match macOS arm64 system headers.
(defcstruct winsize
  (rows :unsigned-short)
  (columns :unsigned-short)
  (x-pixels :unsigned-short)
  (y-pixels :unsigned-short))

(defcstruct termios
  (input-flags :unsigned-long)
  (output-flags :unsigned-long)
  (control-flags :unsigned-long)
  (local-flags :unsigned-long)
  (control-chars (:array :unsigned-char 20))
  (input-speed :unsigned-long)
  (output-speed :unsigned-long))

(defcstruct pollfd
  (fd :int)
  (events :short)
  (revents :short))

(defcfun ("forkpty" %forkpty) :int
  (master :pointer)
  (name :pointer)
  (termios :pointer)
  (winsize :pointer))

(defcfun ("execl" %execl) :int
  (path :string)
  (arg0 :string)
  &rest)

(defcfun ("_exit" %exit) :void
  (status :int))

(defcfun ("read" %read-fd) :long
  (fd :int)
  (buffer :pointer)
  (count :unsigned-long))

(defcfun ("write" %write-fd) :long
  (fd :int)
  (buffer :pointer)
  (count :unsigned-long))

(defcfun ("close" %close-fd) :int
  (fd :int))

(defcfun ("fcntl" %fcntl) :int
  (fd :int)
  (command :int)
  &rest)

(defcfun ("ioctl" %ioctl) :int
  (fd :int)
  (request :unsigned-long)
  &rest)

(defcfun ("poll" %poll) :int
  (fds :pointer)
  (count :unsigned-int)
  (timeout :int))

(defcfun ("waitpid" %waitpid) :int
  (pid :int)
  (status :pointer)
  (options :int))

(defcfun ("kill" %kill) :int
  (pid :int)
  (signal :int))

(defcfun ("isatty" %isatty) :int
  (fd :int))

(defcfun ("tcgetattr" %tcgetattr) :int
  (fd :int)
  (termios :pointer))

(defcfun ("tcsetattr" %tcsetattr) :int
  (fd :int)
  (action :int)
  (termios :pointer))

(defcfun ("cfmakeraw" %cfmakeraw) :void
  (termios :pointer))

(defcfun ("__error" %errno-location) :pointer)

(defcfun ("strerror" %strerror) :string
  (errno :int))

(defconstant +f-getfl+ 3)
(defconstant +f-setfl+ 4)
(defconstant +o-nonblock+ 4)
(defconstant +pollin+ #x0001)
(defconstant +pollout+ #x0004)
(defconstant +pollerr+ #x0008)
(defconstant +pollhup+ #x0010)
(defconstant +pollnval+ #x0020)
(defconstant +wait-nohang+ 1)
(defconstant +tcsanow+ 0)
(defconstant +tiocs-winsz+ #x80087467)
(defconstant +tiocg-winsz+ #x40087468)
(defconstant +eintr+ 4)
(defconstant +eio+ 5)
(defconstant +esrch+ 3)
(defconstant +eagain+ 35)
(defconstant +sighup+ 1)
(defconstant +sigterm+ 15)
(defconstant +sigkill+ 9)

;; These values match macOS arm64 terminal and polling headers.

(define-condition platform-error (error)
  ((operation
    :initarg :operation
    :reader platform-error-operation)
   (errno
    :initarg :errno
    :reader platform-error-errno)
   (message
    :initarg :message
    :reader platform-error-message))
  (:report (lambda (condition stream)
             (format stream "~A failed with errno ~D: ~A"
                     (platform-error-operation condition)
                     (platform-error-errno condition)
                     (platform-error-message condition)))))

(defun current-errno ()
  (mem-ref (%errno-location) :int))

(defun signal-platform-error (operation)
  (let ((errno (current-errno)))
    (error 'platform-error
           :operation operation
           :errno errno
           :message (%strerror errno))))

(defun tty-p (fd)
  (plusp (%isatty fd)))

(defun set-nonblocking (fd)
  (let ((flags (%fcntl fd +f-getfl+)))
    (when (= flags -1)
      (signal-platform-error "fcntl(F_GETFL)"))
    (when (= (%fcntl fd +f-setfl+ :int (logior flags +o-nonblock+)) -1)
      (signal-platform-error "fcntl(F_SETFL)"))))

(defun start-pty (shell width height)
  "Start SHELL inside a non-blocking PTY."
  (check-type shell string)
  (check-type width (integer 1))
  (check-type height (integer 1))
  (with-foreign-object (master :int)
    (with-foreign-object (winsize '(:struct winsize))
      (setf (foreign-slot-value winsize '(:struct winsize) 'rows) height
            (foreign-slot-value winsize '(:struct winsize) 'columns) width
            (foreign-slot-value winsize '(:struct winsize) 'x-pixels) 0
            (foreign-slot-value winsize '(:struct winsize) 'y-pixels) 0)
      (let ((pid (%forkpty master (null-pointer) (null-pointer) winsize)))
        (cond
          ((= pid 0)
           ;; The child keeps the inherited environment and working directory.
           (%execl shell shell :string "-i" :pointer (null-pointer))
           (%exit 127))
          ((minusp pid)
           (signal-platform-error "forkpty"))
          (t
           (let ((fd (mem-ref master :int)))
             (handler-case
                 (progn
                   (set-nonblocking fd)
                   (values fd pid))
               (error (condition)
                 (ignore-errors (%close-fd fd))
                 (ignore-errors (%kill pid +sigkill+))
                 (ignore-errors
                   (with-foreign-object (status :int)
                     (%waitpid pid status 0)))
                 (error condition))))))))))

(defun poll-fds (descriptors &key (timeout -1))
  "Wait for DESCRIPTORS and return their nonzero revents."
  (check-type timeout integer)
  (if (null descriptors)
      nil
      (with-foreign-object (poll-array '(:struct pollfd) (length descriptors))
        (loop for (fd . events) in descriptors
              for index from 0
              for item = (mem-aptr poll-array '(:struct pollfd) index)
              do (setf (foreign-slot-value item '(:struct pollfd) 'fd) fd
                       (foreign-slot-value item '(:struct pollfd) 'events) events
                       (foreign-slot-value item '(:struct pollfd) 'revents) 0))
        (loop for result = (%poll poll-array (length descriptors) timeout)
              do (cond
                   ((plusp result)
                    (return
                      (loop for (fd . events) in descriptors
                            for index from 0
                            for item = (mem-aptr poll-array '(:struct pollfd) index)
                            for revents = (foreign-slot-value item '(:struct pollfd) 'revents)
                            when (plusp revents)
                              collect (cons fd revents))))
                   ((zerop result) (return nil))
                   ((= (current-errno) +eintr+) nil)
                   (t (signal-platform-error "poll")))))))

(defun read-fd (fd &key (max-bytes 4096) (wait-p nil))
  "Read bytes from FD and return bytes plus an end-of-file flag."
  (check-type max-bytes (integer 1))
  (with-foreign-object (buffer `(:array :unsigned-char ,max-bytes))
    (loop for count = (%read-fd fd buffer max-bytes)
          do (cond
               ((plusp count)
                (let ((bytes (make-array count
                                         :element-type '(unsigned-byte 8))))
                  (loop for index below count
                        do (setf (aref bytes index)
                                 (mem-aref buffer :unsigned-char index)))
                  (return (values bytes nil))))
               ((zerop count) (return (values #() t)))
               (t
                (let ((errno (current-errno)))
                  (cond
                    ((= errno +eintr+) nil)
                    ((= errno +eagain+)
                     (if wait-p
                         (poll-fds (list (cons fd +pollin+)) :timeout -1)
                         (return (values #() nil))))
                    ((= errno +eio+) (return (values #() t)))
                    (t (signal-platform-error "read")))))))))

(defun write-fd (fd bytes)
  "Write all BYTES to FD and return the byte count."
  (check-type bytes vector)
  (let ((length (length bytes)))
    (if (zerop length)
        0
        (with-foreign-object (buffer `(:array :unsigned-char ,length))
          (loop for index below length
                do (setf (mem-aref buffer :unsigned-char index)
                         (aref bytes index)))
          (loop with offset = 0
                while (< offset length)
                for count = (%write-fd fd (inc-pointer buffer offset)
                                      (- length offset))
                do (cond
                     ((plusp count) (incf offset count))
                     ((zerop count) (return offset))
                     (t
                      (let ((errno (current-errno)))
                        (cond
                          ((= errno +eintr+) nil)
                          ((= errno +eagain+)
                           (poll-fds (list (cons fd +pollout+)) :timeout -1))
                          (t (signal-platform-error "write"))))))
                finally (return offset))))))

(defun resize-pty (fd width height)
  "Set the PTY terminal size in character cells."
  (check-type width (integer 1))
  (check-type height (integer 1))
  (with-foreign-object (winsize '(:struct winsize))
    (setf (foreign-slot-value winsize '(:struct winsize) 'rows) height
          (foreign-slot-value winsize '(:struct winsize) 'columns) width
          (foreign-slot-value winsize '(:struct winsize) 'x-pixels) 0
          (foreign-slot-value winsize '(:struct winsize) 'y-pixels) 0)
    (when (= (%ioctl fd +tiocs-winsz+ :pointer winsize) -1)
      (signal-platform-error "ioctl(TIOCSWINSZ)")))
  t)

(defun terminal-size (fd)
  "Return FD terminal size as width and height values."
  (with-foreign-object (winsize '(:struct winsize))
    (if (= (%ioctl fd +tiocg-winsz+ :pointer winsize) -1)
        (values nil nil)
        (values (foreign-slot-value winsize '(:struct winsize) 'columns)
                (foreign-slot-value winsize '(:struct winsize) 'rows)))))

(defun wait-process (pid &key (no-hang-p nil))
  "Wait for PID and return its status, or NIL while it runs."
  (with-foreign-object (status :int)
    (loop for result = (%waitpid pid status (if no-hang-p +wait-nohang+ 0))
          do (cond
               ((= result pid) (return (mem-ref status :int)))
               ((zerop result) (return nil))
               ((and (= result -1) (= (current-errno) +eintr+)) nil)
               (t (signal-platform-error "waitpid"))))))

(defun terminate-process (pid)
  "Send SIGHUP to PID."
  (when (and (minusp (%kill pid +sighup+))
             (/= (current-errno) +esrch+))
    (signal-platform-error "kill(SIGHUP)"))
  t)

(defun close-pty (fd)
  "Close the PTY master FD."
  (when (and (minusp (%close-fd fd))
             (/= (current-errno) 9))
    (signal-platform-error "close"))
  t)

(defun call-with-raw-terminal (function &key (fd 0))
  "Run FUNCTION with FD in raw mode and always restore its settings."
  (if (not (tty-p fd))
      (funcall function)
      (with-foreign-object (saved '(:struct termios))
        (when (= (%tcgetattr fd saved) -1)
          (signal-platform-error "tcgetattr"))
        (unwind-protect
            (with-foreign-object (raw '(:struct termios))
              (when (= (%tcgetattr fd raw) -1)
                (signal-platform-error "tcgetattr"))
              (%cfmakeraw raw)
              (when (= (%tcsetattr fd +tcsanow+ raw) -1)
                (signal-platform-error "tcsetattr(raw)"))
              (funcall function))
          ;; Keep the original error when restoration itself fails.
          (when (= (%tcsetattr fd +tcsanow+ saved) -1)
            (warn "Terminal restoration failed with errno ~D."
                  (current-errno)))))))
