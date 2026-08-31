(defpackage #:lispore.platform
  (:use #:cl #:cffi)
  (:export
   #:+pollerr+
   #:+pollhup+
   #:+pollin+
   #:+pollnval+
   #:set-raw-terminal
   #:del-pty
   #:get-poll-events
   #:set-socket-sigpipe
   #:get-fd
   #:set-pty-size
   #:set-close-on-exec
   #:new-pty
   #:get-terminal-size
   #:del-process
   #:get-tty-p
   #:get-process-status
   #:set-fd))

(defpackage #:lispore.utf8
  (:use #:cl)
  (:export
   #:get-utf8-chunk
   #:get-utf8))

(defpackage #:lispore.logging
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:condition-notify
                #:condition-wait
                #:current-thread
                #:join-thread
                #:make-condition-variable
                #:make-lock
                #:make-thread
                #:thread-name
                #:with-lock-held)
  (:export
   #:del-diagnostic-logger
   #:diagnostic-logger
   #:new-diagnostic-logger
   #:set-diagnostic-event))

(defpackage #:lispore.input
  (:use #:cl)
  (:import-from #:lispore.utf8
                #:get-utf8-chunk)
  (:export
   #:input-editor
   #:input-event-text
   #:input-event-type
   #:new-input-editor
   #:get-input-editor-cursor
   #:get-input-editor-text
   #:get-input-completeness
   #:get-input-language
   #:set-input-editor-bytes
   #:set-input-editor-draft
   #:set-input-editor-draft-history
   #:set-input-editor-history
   #:set-input-editor-paste
   #:set-input-editor-submission
   #:del-input-editor
   #:del-input-editor-draft-history))

(defpackage #:lispore.pty
  (:use #:cl)
  (:import-from #:lispore.platform
                #:del-pty
                #:get-fd
                #:set-pty-size
                #:new-pty
                #:del-process
                #:get-process-status
                #:set-fd)
  (:import-from #:lispore.utf8
                #:get-utf8-chunk
                #:get-utf8)
  (:export
   #:del-shell-session
   #:pty-master
   #:get-shell-output
   #:get-shell-output-bytes
   #:set-shell-size
   #:session-eof-p
   #:session-open-p
   #:shell-session
   #:new-shell-session
   #:get-shell-process-status
   #:set-shell-input))

(defpackage #:lispore.terminal
  (:use #:cl)
  (:export
   #:*default-status-line-text*
   #:get-terminal-cell
   #:get-terminal-copy
   #:set-terminal-input
   #:new-terminal-emulator
   #:get-terminal-render
   #:set-terminal-size
   #:screen-cell-character
   #:screen-cell-style
   #:get-terminal-screen-lines
   #:set-terminal-status-line
   #:terminal-emulator
   #:get-terminal-size
   #:get-terminal-cursor-position))

(defpackage #:lispore.session
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:acquire-lock
                #:condition-notify
                #:condition-wait
                #:current-thread
                #:interrupt-thread
                #:join-thread
                #:make-condition-variable
                #:make-lock
                #:make-thread
                #:release-lock
                #:with-lock-held)
  (:import-from #:lispore.input
                #:set-input-editor-draft-history
                #:del-input-editor
                #:del-input-editor-draft-history
                #:get-input-completeness
                #:get-input-editor-cursor
                #:set-input-editor-submission
                #:set-input-editor-draft
                #:set-input-editor-history
                #:get-input-editor-text
                #:get-input-language
                #:new-input-editor)
  (:import-from #:lispore.logging
                #:del-diagnostic-logger
                #:set-diagnostic-event)
  (:import-from #:lispore.pty
                #:del-shell-session
                #:get-shell-output-bytes
                #:new-shell-session
                #:set-shell-input)
  (:import-from #:lispore.terminal
                #:*default-status-line-text*
                #:get-terminal-copy
                #:set-terminal-input
                #:new-terminal-emulator
                #:set-terminal-status-line)
  (:import-from #:lispore.utf8
                #:get-utf8-chunk
                #:get-utf8)
  (:export
   #:attachment
   #:attachment-input-editor
   #:attachment-mode
   #:attachment-session
   #:get-attachment-start-screen
   #:get-attachment-screen
   #:get-attachment-attached-p
   #:del-session-manager
   #:del-session-manager-logger
   #:del-current-session
   #:get-execution-state
   #:set-execution-interruption
   #:get-input-draft
   #:get-input-cursor
   #:get-input-history
   #:set-session-manager-logger
   #:get-session
   #:get-session-by-name
   #:new-session-manager
   #:get-manager-debug-value
   #:set-manager-debug-value
   #:get-manager-debug-enabled-p
   #:set-manager-log
   #:get-attachment-output
   #:get-retained-screen
   #:set-current-session
   #:get-session-running-p
   #:get-session-error
   #:session-id
   #:get-session-list
   #:session-name
   #:set-session-published-output
   #:set-command-submission
   #:del-session
   #:set-input-draft
   #:new-session
   #:set-input-submission))

(defpackage #:lispore.frontend
  (:use #:cl)
  (:import-from #:lispore.platform
                #:+pollerr+
                #:+pollhup+
                #:+pollin+
                #:+pollnval+
                #:set-raw-terminal
                #:get-poll-events
                #:get-fd
                #:get-terminal-size
                #:set-fd)
  (:import-from #:lispore.pty
                #:del-shell-session
                #:pty-master
                #:get-shell-output-bytes
                #:set-shell-size
                #:new-shell-session
                #:get-shell-process-status
                #:set-shell-input)
  (:import-from #:lispore.session
                #:attachment-mode
                #:attachment-input-editor
                #:get-attachment-start-screen
                #:get-attachment-screen
                #:attachment-session
                #:del-current-session
                #:get-execution-state
                #:get-input-cursor
                #:get-input-history
                #:get-input-draft
                #:get-attachment-output
                #:session-id
                #:set-execution-interruption
                #:set-command-submission
                #:set-input-submission)
  (:import-from #:lispore.input
                #:del-input-editor
                #:get-input-completeness
                #:set-input-editor-bytes
                #:get-input-language
                #:set-input-editor-paste
                #:set-input-editor-history
                #:input-event-text
                #:input-event-type)
  (:import-from #:lispore.terminal
                #:*default-status-line-text*
                #:get-terminal-render
                #:set-terminal-status-line)
  (:import-from #:lispore.utf8
                #:get-utf8)
  (:export
   #:set-interactive-shell
   #:set-command-frontend
   #:set-passthrough-frontend))

(defpackage #:lispore
  (:use #:cl)
  (:import-from #:lispore.frontend
                #:set-interactive-shell
                #:set-command-frontend
                #:set-passthrough-frontend)
  (:import-from #:lispore.pty
                #:del-shell-session
                #:pty-master
                #:get-shell-output
                #:get-shell-output-bytes
                #:set-shell-size
                #:session-eof-p
                #:session-open-p
                #:shell-session
                #:new-shell-session
                #:get-shell-process-status
                #:set-shell-input)
  (:import-from #:lispore.session
                #:attachment
                #:attachment-input-editor
                #:attachment-mode
                #:attachment-session
                #:get-attachment-screen
                #:get-attachment-attached-p
                #:get-attachment-start-screen
                #:del-session-manager
                #:del-session-manager-logger
                #:del-current-session
                #:set-execution-interruption
                #:get-input-draft
                #:get-input-cursor
                #:get-input-history
                #:set-session-manager-logger
                #:get-session
                #:get-session-by-name
                #:new-session-manager
                #:get-manager-debug-value
                #:set-manager-debug-value
                #:get-manager-debug-enabled-p
                #:set-manager-log
                #:set-session-published-output
                #:get-attachment-output
                #:get-retained-screen
                #:set-current-session
                #:get-session-running-p
                #:get-session-error
                #:session-id
                #:get-session-list
                #:session-name
                #:get-execution-state
                #:set-input-draft
                #:new-session
                #:set-command-submission
                #:del-session
                #:set-input-submission)
  (:import-from #:lispore.terminal
                #:*default-status-line-text*
                #:get-terminal-cell
                #:set-terminal-input
                #:new-terminal-emulator
                #:get-terminal-copy
                #:get-terminal-render
                #:set-terminal-size
                #:screen-cell-character
                #:screen-cell-style
                #:get-terminal-screen-lines
                #:set-terminal-status-line
                #:terminal-emulator
                #:get-terminal-size
                #:get-terminal-cursor-position)
  (:export
   #:attachment
   #:attachment-input-editor
   #:attachment-mode
   #:attachment-session
   #:get-attachment-attached-p
   #:get-attachment-screen
   #:get-attachment-start-screen
   #:del-current-session
   #:del-shell-session
   #:del-session
   #:del-session-manager
   #:del-session-manager-logger
   #:get-attachment-output
   #:get-execution-state
   #:get-input-cursor
   #:get-input-draft
   #:get-input-history
   #:get-manager-debug-enabled-p
   #:get-manager-debug-value
   #:get-retained-screen
   #:get-session
   #:get-session-by-name
   #:get-session-error
   #:get-session-list
   #:get-session-running-p
   #:get-shell-output
   #:get-shell-output-bytes
   #:get-shell-process-status
   #:get-terminal-cell
   #:get-terminal-copy
   #:get-terminal-cursor-position
   #:get-terminal-render
   #:get-terminal-screen-lines
   #:get-terminal-size
   #:new-session
   #:new-session-manager
   #:new-shell-session
   #:new-terminal-emulator
   #:pty-master
   #:screen-cell-character
   #:screen-cell-style
   #:session-eof-p
   #:session-id
   #:session-name
   #:session-open-p
   #:set-command-frontend
   #:set-command-submission
   #:set-current-session
   #:set-execution-interruption
   #:set-input-draft
   #:set-input-submission
   #:set-interactive-shell
   #:set-manager-debug-value
   #:set-manager-log
   #:set-passthrough-frontend
   #:set-session-manager-logger
   #:set-session-published-output
   #:set-shell-input
   #:set-shell-size
   #:set-terminal-input
   #:set-terminal-size
   #:set-terminal-status-line
   #:shell-session
   #:terminal-emulator))

(defpackage #:lispore.api
  (:import-from #:bordeaux-threads
                #:make-lock
                #:with-lock-held)
  (:export
   #:new
   #:set
   #:get
   #:del))
