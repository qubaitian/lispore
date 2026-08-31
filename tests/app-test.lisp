(in-package #:lispore.tests)

(defun get-application-usage-error (operation)
  "Return the usage error for OPERATION."
  (handler-case
      (progn
        (lispore.app::set-application-usage-error operation)
        nil)
    (error (condition)
      (princ-to-string condition))))

(deftest application-usage-lists-session-manager-forms ()
  (let ((new-usage (get-application-usage-error "new"))
        (del-usage (get-application-usage-error "del")))
    (check (and (search "lispore new session-manager" new-usage)
                (search "lispore new session <name>" new-usage))
           "The NEW usage omits a valid form.")
    (check (and (search "lispore del session-manager" del-usage)
                (search "lispore del session <name>" del-usage))
           "The DEL usage omits a valid form.")))

(deftest application-command-wires-clingon ()
  (let ((command (lispore.app::new-application-command)))
    (check (string= "lispore" (clingon:command-name command))
           "The application command has the wrong name.")
    (check (string= "0.1.0" (clingon:command-version command))
           "The application command has the wrong version.")
    (check (functionp (clingon:command-handler command))
           "The application command has no handler.")
    (check (null (clingon:command-sub-commands command))
           "The application command still exposes subcommands.")
    (check (string= "<new|get|set|del> <path> [value]"
                    (clingon:command-usage command))
           "The application command has the wrong operation usage.")))
