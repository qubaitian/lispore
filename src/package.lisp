(defpackage #:lispore.platform
  (:use #:cl #:cffi)
  (:export
   #:+pollerr+
   #:+pollhup+
   #:+pollin+
   #:+pollnval+
   #:call-with-raw-terminal
   #:close-pty
   #:poll-fds
   #:read-fd
   #:resize-pty
   #:start-pty
   #:terminal-size
   #:terminate-process
   #:tty-p
   #:wait-process
   #:write-fd))

(defpackage #:lispore.utf8
  (:use #:cl)
  (:export
   #:decode-utf8-chunk
   #:encode-utf8))

(defpackage #:lispore.pty
  (:use #:cl)
  (:import-from #:lispore.platform
                #:close-pty
                #:read-fd
                #:resize-pty
                #:start-pty
                #:terminate-process
                #:wait-process
                #:write-fd)
  (:import-from #:lispore.utf8
                #:decode-utf8-chunk
                #:encode-utf8)
  (:export
   #:close-session
   #:pty-master
   #:read-output
   #:read-output-bytes
   #:resize-session
   #:session-eof-p
   #:session-open-p
   #:shell-session
   #:start-shell
   #:wait-for-session
   #:write-input))

(defpackage #:lispore.terminal
  (:use #:cl)
  (:export
   #:cell-at
   #:feed-terminal
   #:make-terminal-emulator
   #:render-terminal
   #:resize-terminal
   #:screen-cell-character
   #:screen-cell-style
   #:screen-lines
   #:set-status-line
   #:terminal-emulator
   #:terminal-size
   #:cursor-position))

(defpackage #:lispore.frontend
  (:use #:cl)
  (:import-from #:lispore.platform
                #:+pollerr+
                #:+pollhup+
                #:+pollin+
                #:+pollnval+
                #:call-with-raw-terminal
                #:poll-fds
                #:read-fd
                #:terminal-size
                #:write-fd)
  (:import-from #:lispore.pty
                #:close-session
                #:pty-master
                #:read-output
                #:read-output-bytes
                #:resize-session
                #:session-eof-p
                #:start-shell
                #:wait-for-session
                #:write-input)
  (:import-from #:lispore.terminal
                #:feed-terminal
                #:make-terminal-emulator
                #:render-terminal
                #:resize-terminal
                #:set-status-line)
  (:import-from #:lispore.utf8
                #:encode-utf8)
  (:export
   #:interactive-shell
   #:run-emulated
   #:run-passthrough))

(defpackage #:lispore
  (:use #:cl)
  (:import-from #:lispore.frontend
                #:interactive-shell
                #:run-emulated
                #:run-passthrough)
  (:import-from #:lispore.pty
                #:close-session
                #:pty-master
                #:read-output
                #:read-output-bytes
                #:resize-session
                #:session-eof-p
                #:session-open-p
                #:shell-session
                #:start-shell
                #:wait-for-session
                #:write-input)
  (:import-from #:lispore.terminal
                #:cell-at
                #:feed-terminal
                #:make-terminal-emulator
                #:render-terminal
                #:resize-terminal
                #:screen-cell-character
                #:screen-cell-style
                #:screen-lines
                #:set-status-line
                #:terminal-emulator
                #:terminal-size
                #:cursor-position)
  (:export
   #:cell-at
   #:close-session
   #:cursor-position
   #:feed-terminal
   #:interactive-shell
   #:make-terminal-emulator
   #:pty-master
   #:read-output
   #:read-output-bytes
   #:render-terminal
   #:resize-terminal
   #:resize-session
   #:run-emulated
   #:run-passthrough
   #:screen-cell-character
   #:screen-cell-style
   #:screen-lines
   #:set-status-line
   #:session-eof-p
   #:session-open-p
   #:shell-session
   #:start-shell
   #:terminal-emulator
   #:terminal-size
   #:wait-for-session
   #:write-input))
