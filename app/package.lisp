(defpackage #:lispore.app
  (:use #:cl)
  (:import-from #:clingon
                #:command-arguments
                #:make-command
                #:run)
  (:import-from #:lispore
                #:del-current-session
                #:del-session
                #:del-session-manager
                #:del-session-manager-logger
                #:get-manager-debug-value
                #:get-manager-debug-enabled-p
                #:get-session
                #:get-session-by-name
                #:get-session-list
                #:new-session
                #:new-session-manager
                #:session-id
                #:session-name
                #:set-current-session
                #:set-interactive-shell
                #:set-manager-debug-value
                #:set-manager-log
                #:set-session-manager-logger
                #:set-session-published-output)
  (:import-from #:lispore.logging
                #:del-diagnostic-logger
                #:new-diagnostic-logger)
  (:import-from #:lispore.platform
                #:+pollerr+
                #:+pollhup+
                #:+pollin+
                #:+pollnval+
                #:del-pty
                #:get-fd
                #:get-poll-events
                #:get-terminal-size
                #:set-close-on-exec
                #:set-fd
                #:set-raw-terminal
                #:set-socket-sigpipe)
  (:import-from #:lispore.utf8
                #:get-utf8)
  (:import-from #:bordeaux-threads
                #:make-thread)
  (:export
   #:main))
