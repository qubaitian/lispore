(defpackage #:lispore.tests
  (:use #:cl)
  (:import-from #:lispore.input
                #:input-editor-add-draft-history
                #:input-editor-cursor
                #:input-editor-clear
                #:input-completeness
                #:input-editor-feed
                #:input-editor-record-submission
                #:input-editor-paste
                #:input-editor-set-history
                #:input-editor-text
                #:input-event-text
                #:input-event-type
                #:input-language
                #:make-input-editor)
  (:import-from #:lispore
                #:attach-session
                #:attachment-attached-p
                #:attachment-mode
                #:attachment-screen
                #:close-session
                #:close-session-manager
                #:cell-at
                #:cursor-position
                #:detach
                #:execution-state
                #:find-or-create-session
                #:feed-terminal
                #:interrupt-execution
                #:input-draft
                #:input-history
                #:install-session-manager-logger
                #:lookup-session
                #:lookup-session-by-name
                #:make-session-manager
                #:publish-session-output
                #:run-passthrough
                #:make-terminal-emulator
                #:read-output
                #:read-attachment
                #:retained-screen
                #:resize-terminal
                #:resize-session
                #:render-terminal
                #:run-command
                #:screen-lines
                #:screen-cell-character
                #:screen-cell-style
                #:session-running-p
                #:session-id
                #:session-list
                #:session-name
                #:restore-session
                #:reattach-session
                #:set-input-draft
                #:set-status-line
                #:session-open-p
                #:start-session
                #:start-shell
                #:submit-command
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
                #:thread-alive-p
                #:with-lock-held)
  (:import-from #:lispore.utf8
                #:decode-utf8-chunk
                #:encode-utf8)
  (:import-from #:lispore.logging
                #:close-diagnostic-logger
                #:log-diagnostic-event
                #:make-diagnostic-logger)
  (:import-from #:lispore.platform
                #:call-with-raw-terminal
                #:close-pty
                #:+pollin+
                #:poll-fds
                #:read-fd
                #:tty-p
                #:write-fd)
  (:export #:run-tests))
