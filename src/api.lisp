(in-package #:lispore.api)

(cl:defvar *api-lock* (make-lock "lispore api"))
(cl:defvar *api-state* (cl:list :manager cl:nil :current-session cl:nil))

(cl:defun get-api-manager (manager)
  "Return MANAGER or the process-local default manager."
  (cl:or manager
         (with-lock-held (*api-lock*)
           (cl:or (cl:getf *api-state* :manager)
                  (cl:setf (cl:getf *api-state* :manager)
                           (lispore:new-session-manager))))))

(cl:defun get-api-current-session ()
  "Return the current Session for this API caller."
  (with-lock-held (*api-lock*)
    (cl:getf *api-state* :current-session)))

(cl:defun set-api-current-session (session)
  "Set the current Session for this API caller."
  (with-lock-held (*api-lock*)
    (cl:setf (cl:getf *api-state* :current-session) session)))

(cl:defun new-api-session (manager name)
  "Create and return the named Session value."
  (cl:check-type name cl:string)
  (cl:let ((session-id (lispore:new-session manager
                                            :name name
                                            :mode :command)))
    (cl:or (lispore:get-session manager session-id)
           (cl:error "The new Session is not registered."))))

(cl:defun set-api-debug (manager value)
  "Set the manager debug value."
  (cl:check-type value cl:integer)
  (lispore:set-manager-debug-value manager value))

(cl:defun set-api-current-session-position (manager name)
  "Enter NAME and return its Session value after frontend exit."
  (cl:check-type name cl:string)
  (cl:let ((session (lispore:get-session-by-name manager name)))
    (cl:unless session
      (cl:error "The Session named ~A does not exist." name))
    (cl:let ((attachment (lispore:set-current-session
                          manager
                          (lispore:session-id session)
                          :mode :command)))
      (cl:unless attachment
        (cl:error "The Session named ~A cannot accept an attachment." name))
      (set-api-current-session session)
      (cl:unwind-protect
           (lispore:set-interactive-shell :attachment attachment)
        (lispore:del-current-session attachment)))
    session))

(cl:defun get-api-current-session-name ()
  "Return the current Session name for this API caller."
  (cl:let ((session (get-api-current-session)))
    (cl:or session
           (cl:error "The current-session position is missing."))
    (lispore:session-name session)))

(cl:defun del-api-session (manager name)
  "Delete the named Session and return its value."
  (cl:check-type name cl:string)
  (cl:let ((session (lispore:get-session-by-name manager name)))
    (cl:unless session
      (cl:error "The Session named ~A does not exist." name))
    (lispore:del-session manager (lispore:session-id session))
    (with-lock-held (*api-lock*)
      (cl:when (cl:eq session (cl:getf *api-state* :current-session))
        (cl:setf (cl:getf *api-state* :current-session) cl:nil)))
    session))

(cl:defun get-api-del-arguments (arguments)
  "Return the optional deletion value and manager."
    (cl:let ((value cl:nil)
           (manager cl:nil))
    (cl:when (cl:and arguments
                     (cl:not (cl:keywordp (cl:first arguments))))
      (cl:setf value (cl:pop arguments)))
    (cl:when arguments
      (cl:unless (cl:and (cl:= (cl:length arguments) 2)
                         (cl:eq (cl:first arguments) :manager))
        (cl:error "The data operation accepts only a manager option."))
      (cl:setf manager (cl:second arguments)))
    (cl:values value manager)))

(cl:defun new (path value cl:&key manager)
  "Create the value at missing PATH."
  (cl:case path
    (:session
     (new-api-session (get-api-manager manager) value))
    (cl:otherwise
     (cl:error "NEW does not support position ~S." path))))

(cl:defun set (path value cl:&key manager)
  "Set the existing value at PATH."
  (cl:case path
    (:debug
     (set-api-debug (get-api-manager manager) value))
    (:current-session
     (set-api-current-session-position (get-api-manager manager) value))
    (cl:otherwise
     (cl:error "SET does not support position ~S." path))))

(cl:defun get (path cl:&key manager)
  "Return the existing value at PATH."
  (cl:case path
    (:session
     (lispore:get-session-list (get-api-manager manager)))
    (:debug
     (lispore:get-manager-debug-value (get-api-manager manager)))
    (:current-session
     (get-api-current-session-name))
    (cl:otherwise
     (cl:error "GET does not support position ~S." path))))

(cl:defun del (path cl:&rest arguments)
  "Delete the value at PATH."
  (cl:multiple-value-bind (value manager)
      (get-api-del-arguments arguments)
  (cl:case path
      (:session
       (cl:unless value
         (cl:error "DEL SESSION requires a Session name."))
       (del-api-session (get-api-manager manager) value))
      ((:debug :current-session)
       (cl:error "DEL cannot remove protected position ~S." path))
      (cl:otherwise
       (cl:error "DEL does not support position ~S." path)))))
