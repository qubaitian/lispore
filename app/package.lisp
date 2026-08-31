(defpackage #:lispore.app
  (:use #:cl)
  (:import-from #:clingon
                #:command-arguments
                #:make-command
                #:run)
  (:import-from #:lispore
                #:attach-session
                #:close-session-manager
                #:detach
                #:find-or-create-session
                #:install-session-manager-logger
                #:interactive-shell
                #:make-session-manager
                #:manager-debug-enabled-p
                #:manager-log
                #:publish-session-output
                #:lookup-session-by-name
                #:session-list
                #:session-id)
  (:import-from #:lispore.logging
                #:close-diagnostic-logger
                #:make-diagnostic-logger)
  (:import-from #:lispore.platform
                #:+pollerr+
                #:+pollhup+
                #:+pollin+
                #:+pollnval+
                #:call-with-raw-terminal
                #:poll-fds
                #:prevent-socket-sigpipe
                #:read-fd
                #:set-close-on-exec
                #:terminal-size
                #:write-fd)
  (:import-from #:lispore.utf8
                #:encode-utf8)
  (:import-from #:bordeaux-threads
                #:make-thread)
  (:export
   #:main))
