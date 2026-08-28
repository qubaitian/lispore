(defpackage #:lispore.tests
  (:use #:cl)
  (:import-from #:lispore
                #:attach-session
                #:attachment-attached-p
                #:attachment-screen
                #:close-session
                #:close-session-manager
                #:cell-at
                #:cursor-position
                #:detach
                #:feed-terminal
                #:input-draft
                #:lookup-session
                #:make-session-manager
                #:run-emulated
                #:run-passthrough
                #:make-terminal-emulator
                #:read-output
                #:read-attachment
                #:retained-screen
                #:resize-terminal
                #:resize-session
                #:render-terminal
                #:screen-lines
                #:screen-cell-character
                #:screen-cell-style
                #:session-running-p
                #:restore-session
                #:set-input-draft
                #:set-status-line
                #:session-open-p
                #:start-session
                #:start-shell
                #:submit-input
                #:terminate-session
                #:write-input)
  (:import-from #:bordeaux-threads
                #:condition-notify
                #:condition-wait
                #:join-thread
                #:make-condition-variable
                #:make-lock
                #:make-thread
                #:with-lock-held)
  (:import-from #:lispore.utf8
                #:decode-utf8-chunk
                #:encode-utf8)
  (:import-from #:lispore.platform
                #:call-with-raw-terminal
                #:close-pty
                #:+pollin+
                #:poll-fds
                #:read-fd
                #:tty-p)
  (:export #:run-tests))
