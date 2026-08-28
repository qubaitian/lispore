(defpackage #:lispore.app
  (:use #:cl)
  (:import-from #:clingon
                #:command-arguments
                #:make-command
                #:run)
  (:import-from #:lispore
                #:attach-session
                #:close-session-manager
                #:interactive-shell
                #:make-session-manager
                #:start-session)
  (:import-from #:lispore.platform
                #:terminal-size)
  (:export
   #:main))
