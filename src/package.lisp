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

(defpackage #:lispore.input
  (:use #:cl)
  (:import-from #:lispore.utf8
                #:decode-utf8-chunk)
  (:export
   #:input-editor
   #:input-editor-add-draft-history
   #:input-editor-clear
   #:input-editor-clear-draft-history
   #:input-editor-cursor
   #:input-editor-feed
   #:input-completeness
   #:input-editor-paste
   #:input-editor-record-submission
   #:input-editor-set-draft
   #:input-editor-set-history
   #:input-editor-text
   #:input-language
   #:input-event-text
   #:input-event-type
   #:make-input-editor))

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
   #:*default-status-line-text*
   #:cell-at
   #:copy-terminal
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
                #:input-editor-add-draft-history
                #:input-editor-clear
                #:input-editor-clear-draft-history
                #:input-completeness
                #:input-editor-cursor
                #:input-editor-record-submission
                #:input-editor-set-draft
                #:input-editor-set-history
                #:input-editor-text
                #:input-language
                #:make-input-editor)
  (:import-from #:lispore.pty
                #:close-session
                #:read-output-bytes
                #:start-shell
                #:write-input)
  (:import-from #:lispore.terminal
                #:*default-status-line-text*
                #:copy-terminal
                #:feed-terminal
                #:make-terminal-emulator
                #:set-status-line)
  (:import-from #:lispore.utf8
                #:decode-utf8-chunk
                #:encode-utf8)
  (:export
   #:attach-session
   #:attachment
   #:attachment-input-editor
   #:attachment-mode
   #:attachment-session
   #:attachment-start-screen
   #:attachment-screen
   #:attachment-attached-p
   #:close-session-manager
   #:detach
   #:execution-state
   #:interrupt-execution
   #:input-draft
   #:input-cursor
   #:input-history
   #:lookup-session
   #:make-session-manager
   #:read-attachment
   #:retained-screen
   #:restore-session
   #:reattach-session
   #:session-running-p
   #:session-error
   #:session-id
   #:publish-session-output
   #:submit-command
   #:terminate-session
   #:set-input-draft
   #:start-session
   #:submit-input))

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
                #:read-output-bytes
                #:resize-session
                #:start-shell
                #:wait-for-session
                #:write-input)
  (:import-from #:lispore.session
                #:attachment-mode
                #:attachment-input-editor
                #:attachment-start-screen
                #:attachment-screen
                #:attachment-session
                #:detach
                #:execution-state
                #:input-cursor
                #:input-history
                #:input-draft
                #:read-attachment
                #:session-id
                #:interrupt-execution
                #:submit-command
                #:submit-input)
  (:import-from #:lispore.input
                #:input-editor-clear
                #:input-completeness
                #:input-editor-feed
                #:input-language
                #:input-editor-paste
                #:input-editor-set-history
                #:input-event-text
                #:input-event-type)
  (:import-from #:lispore.terminal
                #:*default-status-line-text*
                #:render-terminal
                #:set-status-line)
  (:import-from #:lispore.utf8
                #:encode-utf8)
  (:export
   #:interactive-shell
   #:run-command
   #:run-passthrough))

(defpackage #:lispore
  (:use #:cl)
  (:import-from #:lispore.frontend
                #:interactive-shell
                #:run-command
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
  (:import-from #:lispore.session
                #:attach-session
                #:attachment-mode
                #:attachment-input-editor
                #:attachment-session
                #:attachment-screen
                #:attachment-attached-p
                #:close-session-manager
                #:detach
                #:interrupt-execution
                #:input-draft
                #:input-cursor
                #:input-history
                #:lookup-session
                #:make-session-manager
                #:read-attachment
                #:retained-screen
                #:restore-session
                #:reattach-session
                #:session-running-p
                #:session-error
                #:session-id
                #:execution-state
                #:set-input-draft
                #:start-session
                #:submit-command
                #:terminate-session
                #:submit-input)
  (:import-from #:lispore.terminal
                #:*default-status-line-text*
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
   #:attach-session
   #:attachment-attached-p
   #:attachment-input-editor
   #:attachment-mode
   #:attachment-screen
   #:attachment-session
   #:cell-at
   #:close-session
   #:close-session-manager
   #:cursor-position
   #:detach
   #:feed-terminal
   #:input-draft
   #:input-cursor
   #:input-history
   #:interrupt-execution
   #:interactive-shell
   #:lookup-session
   #:make-terminal-emulator
   #:make-session-manager
   #:pty-master
   #:read-output
   #:read-output-bytes
   #:read-attachment
   #:render-terminal
   #:resize-terminal
   #:resize-session
   #:retained-screen
   #:restore-session
   #:reattach-session
   #:run-command
   #:run-passthrough
   #:screen-cell-character
   #:screen-cell-style
   #:screen-lines
   #:set-status-line
   #:session-eof-p
   #:session-running-p
   #:session-error
   #:execution-state
   #:session-id
   #:session-open-p
   #:shell-session
   #:set-input-draft
   #:start-session
   #:start-shell
   #:submit-input
   #:submit-command
   #:terminate-session
   #:terminal-emulator
   #:terminal-size
   #:wait-for-session
   #:write-input))
