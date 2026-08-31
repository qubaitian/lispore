(defpackage #:lispore.tests
  (:use #:cl)
  (:import-from #:lispore.input
                #:set-input-editor-draft-history
                #:get-input-editor-cursor
                #:del-input-editor
                #:get-input-completeness
                #:set-input-editor-bytes
                #:set-input-editor-submission
                #:set-input-editor-paste
                #:set-input-editor-history
                #:get-input-editor-text
                #:input-event-text
                #:input-event-type
                #:get-input-language
                #:new-input-editor)
  (:import-from #:lispore
                #:set-current-session
                #:get-attachment-attached-p
                #:attachment-mode
                #:get-attachment-screen
                #:del-shell-session
                #:del-session-manager
                #:get-terminal-cell
                #:get-terminal-cursor-position
                #:del-current-session
                #:get-execution-state
                #:set-terminal-input
                #:set-execution-interruption
                #:get-input-draft
                #:get-input-history
                #:set-session-manager-logger
                #:get-session
                #:get-session-by-name
                #:new-session-manager
                #:set-passthrough-frontend
                #:new-terminal-emulator
                #:get-shell-output
                #:get-attachment-output
                #:get-retained-screen
                #:set-terminal-size
                #:set-shell-size
                #:get-terminal-render
                #:set-command-frontend
                #:get-terminal-screen-lines
                #:set-session-published-output
                #:screen-cell-character
                #:screen-cell-style
                #:get-session-running-p
                #:session-id
                #:get-session-list
                #:session-name
                #:set-input-draft
                #:set-terminal-status-line
                #:session-open-p
                #:new-session
                #:new-shell-session
                #:set-command-submission
                #:set-input-submission
                #:del-session
                #:set-shell-input)
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
                #:get-utf8-chunk
                #:get-utf8)
  (:import-from #:lispore.logging
                #:del-diagnostic-logger
                #:set-diagnostic-event
                #:new-diagnostic-logger)
  (:import-from #:lispore.platform
                #:set-raw-terminal
                #:del-pty
                #:+pollin+
                #:get-poll-events
                #:get-fd
                #:get-tty-p
                #:set-fd)
  (:export #:set-tests))
