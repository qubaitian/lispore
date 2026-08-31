(in-package #:lispore.tests)

(deftest api-exports-independent-four-operations ()
  (let ((api-set (find-symbol "SET" "LISPORE.API"))
        (api-get (find-symbol "GET" "LISPORE.API")))
    (check (and api-set api-get)
           "The data interface does not export SET and GET.")
    (check (not (eq api-set (find-symbol "SET" "COMMON-LISP")))
           "The data interface reuses Common Lisp SET.")
    (check (not (eq api-get (find-symbol "GET" "COMMON-LISP")))
           "The data interface reuses Common Lisp GET.")))

(defun get-api-error-p (function)
  "Return true when FUNCTION signals an error."
  (handler-case
      (progn
        (funcall function)
        nil)
    (error () t)))

(deftest api-manages-session-debug-and-deletion-positions ()
  (let ((manager (new-session-manager :retention-seconds 5)))
    (unwind-protect
         (let ((session
                 (lispore.api:new :session "api-session" :manager manager)))
           (check (eq session (get-session-by-name manager "api-session"))
                  "NEW does not retain the live Session value.")
           (check (equal '(("api-session" . :ready))
                        (lispore.api:get :session :manager manager))
                  "GET SESSION does not return the Session list.")
           (check (= 0 (lispore.api:get :debug :manager manager))
                  "Debug does not start at zero.")
           (check (= 1 (lispore.api:set :debug 1 :manager manager))
                  "SET DEBUG does not return one.")
           (check (= 1 (lispore.api:get :debug :manager manager))
                  "GET DEBUG does not return one.")
           (check (eq session
                      (lispore.api:del :session "api-session"
                                       :manager manager))
                  "DEL SESSION does not return the deleted value.")
           (check (null (get-session-by-name manager "api-session"))
                  "DEL SESSION keeps the registry position."))
      (del-session-manager manager))))

(deftest api-rejects-invalid-position-transitions ()
  (let ((manager (new-session-manager :retention-seconds 5)))
    (unwind-protect
         (progn
           (check (get-api-error-p
                   (lambda () (lispore.api:get :missing :manager manager)))
                  "GET accepted a missing position.")
           (check (get-api-error-p
                   (lambda () (lispore.api:del :debug :manager manager)))
                  "DEL accepted the protected debug position.")
           (check (get-api-error-p
                   (lambda () (lispore.api:new :debug 1 :manager manager)))
                  "NEW accepted the existing debug position."))
      (del-session-manager manager))))
