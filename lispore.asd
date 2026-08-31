(asdf:defsystem "lispore"
  :description "Lispore provides a small Common Lisp terminal tool."
  :version "0.1.0"
  :depends-on ("bordeaux-threads" "cffi")
  :serial t
  :components ((:file "src/package")
               (:file "src/utf8")
               (:file "src/input")
               (:file "src/platform")
               (:file "src/pty")
               (:file "src/terminal")
               (:file "src/session")
               (:file "src/frontend"))
  :in-order-to ((test-op (test-op "lispore/tests"))))

(asdf:defsystem "lispore/app"
  :description "The Lispore command-line application."
  :version "0.1.0"
  :depends-on ("lispore" "clingon" "usocket")
  :serial t
  :components ((:file "app/package")
               (:file "app/manager")
               (:file "app/main"))
  :build-operation "program-op"
  :build-pathname "bin/lispore"
  :entry-point "lispore.app:main")

(asdf:defsystem "lispore/tests"
  :depends-on ("lispore" "lispore/app")
  :serial t
  :components ((:file "tests/package")
               (:file "tests/test")
               (:file "tests/app-test"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call :lispore.tests :run-tests)))
