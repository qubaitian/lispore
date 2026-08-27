(defpackage #:lispore.tests
  (:use #:cl)
  (:import-from #:lispore
                #:close-session
                #:cell-at
                #:cursor-position
                #:feed-terminal
                #:run-emulated
                #:run-passthrough
                #:make-terminal-emulator
                #:read-output
                #:resize-session
                #:render-terminal
                #:screen-lines
                #:screen-cell-character
                #:screen-cell-style
                #:session-open-p
                #:start-shell
                #:write-input)
  (:import-from #:lispore.utf8
                #:decode-utf8-chunk
                #:encode-utf8)
  (:import-from #:lispore.platform
                #:call-with-raw-terminal
                #:tty-p)
  (:export #:run-tests))
