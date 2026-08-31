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

(deftest api-manages-session-manager-lifecycle ()
  (when (eq :running (lispore.api:get :session-manager))
    (lispore.api:del :session-manager))
  (unwind-protect
       (progn
         (check (eq :stopped (lispore.api:get :session-manager))
                "GET SESSION-MANAGER does not report stopped.")
         (let* ((manager (lispore.api:new :session-manager))
                (session (lispore.api:new :session "api-session")))
           (check (eq :running (lispore:get-session-manager-state manager))
                  "NEW SESSION-MANAGER does not start the manager.")
           (check (get-session-running-p session)
                  "NEW SESSION does not use the running default manager.")
           (check (eq :running (lispore.api:get :session-manager))
                  "GET SESSION-MANAGER does not report running.")
           (check (get-api-error-p
                   (lambda () (lispore.api:new :session-manager)))
                  "NEW SESSION-MANAGER accepts a duplicate.")
           (check (eq manager (lispore.api:del :session-manager))
                  "DEL SESSION-MANAGER returns the wrong manager.")
           (check (not (get-session-running-p session))
                  "DEL SESSION-MANAGER leaves a Session running.")
           (check (eq :stopped (lispore:get-session-manager-state manager))
                  "DEL SESSION-MANAGER does not stop the manager.")
           (check (eq :stopped (lispore.api:get :session-manager))
                  "GET SESSION-MANAGER does not report stopped after DEL.")
           (check (get-api-error-p
                   (lambda () (lispore.api:new :session "api-session")))
                  "NEW SESSION accepts a stopped default manager.")
           (check (get-api-error-p
                   (lambda () (lispore.api:del :session-manager)))
                  "DEL SESSION-MANAGER accepts a missing manager.")))
    (when (eq :running (lispore.api:get :session-manager))
      (lispore.api:del :session-manager))))

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
