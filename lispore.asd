(asdf:defsystem "lispore"
  :description "Lispore provides a small Common Lisp terminal tool."
  :version "0.1.0"
  :depends-on ("bordeaux-threads" "cffi")
  :serial t
  :components ((:file "src/package")
               (:file "src/utf8")
               (:file "src/platform")
               (:file "src/pty")
               (:file "src/terminal")
               (:file "src/session")
               (:file "src/frontend"))
  :in-order-to ((test-op (test-op "lispore/tests"))))

(asdf:defsystem "lispore/tests"
  :depends-on ("lispore")
  :serial t
  :components ((:file "tests/package")
               (:file "tests/test"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call :lispore.tests :run-tests)))
