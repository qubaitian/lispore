(in-package #:lispore.input)

(defstruct (input-event
            (:constructor new-input-event (type &optional text)))
  type
  text)

(defclass input-editor ()
  ((text
    :initform ""
    :accessor editor-text)
   (cursor
    :initform 0
    :accessor editor-cursor)
   (history
    :initarg :history
    :initform nil
    :accessor editor-history)
   (draft-history
    :initform nil
    :accessor editor-draft-history)
   (history-index
    :initform nil
    :accessor editor-history-index)
   (history-base-text
    :initform ""
    :accessor editor-history-base-text)
   (decode-pending
    :initform nil
    :accessor editor-decode-pending)
   (parser-state
    :initform :ground
    :accessor editor-parser-state)
   (csi-buffer
    :initform ""
    :accessor editor-csi-buffer)
   (paste-buffer
    :initform nil
    :accessor editor-paste-buffer)))

(defun get-history-copy (history)
  "Return independent strings from newest-first HISTORY."
  (mapcar #'copy-seq history))

(defun new-input-editor (&key (history nil))
  "Create an input editor with HISTORY in newest-first order."
  (make-instance 'input-editor :history (get-history-copy history)))

(defun get-input-editor-text (editor)
  "Return a copy of EDITOR's input draft."
  (check-type editor input-editor)
  (copy-seq (editor-text editor)))

(defun get-input-editor-cursor (editor)
  "Return EDITOR's character cursor position."
  (check-type editor input-editor)
  (editor-cursor editor))

(defun set-input-editor-draft (editor text &key (cursor-at-end-p t))
  "Set EDITOR's draft and optionally place its cursor at the end."
  (check-type editor input-editor)
  (check-type text string)
  (setf (editor-text editor) (copy-seq text)
        (editor-cursor editor)
        (if cursor-at-end-p
            (length text)
            (min (editor-cursor editor) (length text)))
        (editor-history-index editor) nil)
  editor)

(defun del-input-editor (editor)
  "Clear EDITOR's draft and its history navigation state."
  (set-input-editor-draft editor "")
  (setf (editor-history-base-text editor) "")
  editor)

(defun del-input-editor-draft-history (editor)
  "Remove EDITOR's private recovery drafts."
  (check-type editor input-editor)
  (setf (editor-draft-history editor) nil
        (editor-history-index editor) nil
        (editor-history-base-text editor) "")
  editor)

(defun set-input-editor-history (editor history)
  "Replace EDITOR's input history with newest-first HISTORY."
  (check-type editor input-editor)
  (setf (editor-history editor) (get-history-copy history))
  editor)

(defun set-input-editor-submission (editor text)
  "Retain one submitted TEXT in EDITOR's input history."
  (check-type editor input-editor)
  (check-type text string)
  (unless (zerop (length text))
    (push (copy-seq text) (editor-history editor)))
  (setf (editor-history-index editor) nil)
  editor)

(defun set-input-editor-draft-history (editor text)
  "Retain one unsubmitted recovery TEXT for EDITOR."
  (check-type editor input-editor)
  (check-type text string)
  (unless (zerop (length text))
    (push (copy-seq text) (editor-draft-history editor)))
  editor)

(defun set-input-editor-history-live (editor)
  (setf (editor-history-index editor) nil)
  editor)

(defun set-input-editor-text (editor text)
  (setf (editor-text editor) (copy-seq text)
        (editor-cursor editor) (length text)))

(defun get-input-editor-all-history (editor)
  (append (editor-draft-history editor)
          (editor-history editor)))

(defun set-input-editor-history-previous (editor)
  (let ((history (get-input-editor-all-history editor)))
    (when (plusp (length history))
      (unless (editor-history-index editor)
        (setf (editor-history-base-text editor)
              (copy-seq (editor-text editor))
              (editor-history-index editor) -1))
      (let ((next-index (1+ (editor-history-index editor))))
        (when (< next-index (length history))
          (setf (editor-history-index editor) next-index)
          (set-input-editor-text editor (nth next-index history)))))))

(defun set-input-editor-history-next (editor)
  (when (editor-history-index editor)
    (if (plusp (editor-history-index editor))
        (let ((next-index (1- (editor-history-index editor))))
          (setf (editor-history-index editor) next-index)
          (set-input-editor-text editor
                               (nth next-index (get-input-editor-all-history editor))))
        (progn
          (set-input-editor-text editor (editor-history-base-text editor))
          (setf (editor-history-index editor) nil))))
  editor)

(defun set-input-editor-insert-character (editor character)
  (set-input-editor-history-live editor)
  (let ((cursor (editor-cursor editor))
        (text (editor-text editor)))
    (setf (editor-text editor)
          (concatenate 'string
                       (subseq text 0 cursor)
                       (string character)
                       (subseq text cursor))
          (editor-cursor editor) (1+ cursor))))

(defun get-normalized-input-text (text)
  "Return TEXT with carriage returns changed to newlines."
  (with-output-to-string (stream)
    (loop for character across text
          do (write-char (if (char= character #\Return)
                             #\Newline
                             character)
                         stream))))

(defun set-input-editor-insert-text (editor text)
  (let ((text (get-normalized-input-text text)))
    (unless (zerop (length text))
      (set-input-editor-history-live editor)
      (let ((cursor (editor-cursor editor))
            (old-text (editor-text editor)))
        (setf (editor-text editor)
              (concatenate 'string
                           (subseq old-text 0 cursor)
                           text
                           (subseq old-text cursor))
              (editor-cursor editor) (+ cursor (length text))))))
  editor)

(defun del-input-editor-before-cursor (editor)
  (when (plusp (editor-cursor editor))
    (set-input-editor-history-live editor)
    (let ((cursor (editor-cursor editor))
          (text (editor-text editor)))
      (setf (editor-text editor)
            (concatenate 'string
                         (subseq text 0 (1- cursor))
                         (subseq text cursor))
            (editor-cursor editor) (1- cursor)))))

(defun del-input-editor-at-cursor (editor)
  (when (< (editor-cursor editor) (length (editor-text editor)))
    (set-input-editor-history-live editor)
    (let ((cursor (editor-cursor editor))
          (text (editor-text editor)))
      (setf (editor-text editor)
            (concatenate 'string
                         (subseq text 0 cursor)
                         (subseq text (1+ cursor)))))))

(defun set-input-editor-cursor (editor amount)
  (set-input-editor-history-live editor)
  (setf (editor-cursor editor)
        (max 0 (min (length (editor-text editor))
                    (+ (editor-cursor editor) amount))))
  editor)

(defun set-input-editor-csi (editor final)
  (let ((parameters (editor-csi-buffer editor)))
    (case final
      (#\A (set-input-editor-history-previous editor))
      (#\B (set-input-editor-history-next editor))
      (#\C (set-input-editor-cursor editor 1))
      (#\D (set-input-editor-cursor editor -1))
      (#\H (set-input-editor-history-live editor)
            (setf (editor-cursor editor) 0))
      (#\F (set-input-editor-history-live editor)
            (setf (editor-cursor editor) (length (editor-text editor))))
      (#\~
       (cond
         ((member parameters '("1" "7") :test #'string=)
          (set-input-editor-history-live editor)
          (setf (editor-cursor editor) 0))
         ((member parameters '("4" "8") :test #'string=)
          (set-input-editor-history-live editor)
          (setf (editor-cursor editor) (length (editor-text editor))))
         ((string= "3" parameters)
          (del-input-editor-at-cursor editor))
         ((string= "200" parameters)
          (setf (editor-parser-state editor) :paste
                (editor-paste-buffer editor) nil))))))
  (setf (editor-csi-buffer editor) "")
  editor)

(defun set-input-editor-paste-text (editor text)
  "Append TEXT to EDITOR's pending paste in reverse order."
  (loop for character across text
        do (push character (editor-paste-buffer editor)))
  editor)

(defun set-input-editor-paste-finish (editor)
  "Insert EDITOR's pending paste and leave paste mode."
  (set-input-editor-insert-text editor
               (coerce (nreverse (editor-paste-buffer editor)) 'string))
  (setf (editor-paste-buffer editor) nil
        (editor-parser-state editor) :ground)
  editor)

(defun set-input-editor-paste-character (editor character)
  (let ((code (char-code character)))
    (cond
      ((and (<= (char-code #\@) code) (<= code (char-code #\~)))
       (if (and (char= character #\~)
                (string= "201" (editor-csi-buffer editor)))
           (set-input-editor-paste-finish editor)
           (progn
             (set-input-editor-paste-text
              editor
              (concatenate 'string
                           (string #\Escape)
                           "["
                           (editor-csi-buffer editor)
                           (string character)))
             (setf (editor-parser-state editor) :paste))))
      ((or (<= (char-code #\0) code (char-code #\?))
           (<= (char-code #\Space) code (char-code #\/)))
       (setf (editor-csi-buffer editor)
             (concatenate 'string
                          (editor-csi-buffer editor)
                          (string character))))
      (t
       (set-input-editor-paste-text
        editor
        (concatenate 'string
                     (string #\Escape)
                     "["
                     (editor-csi-buffer editor)
                     (string character)))
       (setf (editor-parser-state editor) :paste))))
  nil)

(defun set-input-editor-character (editor character)
  ;; Keep escape parsing separate from ordinary text editing.
  (cond
    ((char= character (code-char 3))
     ;; Control-C always interrupts, even inside an escape sequence.
     (setf (editor-parser-state editor) :ground
           (editor-csi-buffer editor) ""
           (editor-paste-buffer editor) nil)
     (list (new-input-event :interrupt)))
    ((char= character (code-char 4))
     ;; Control-D closes only when no draft exists.
     (setf (editor-parser-state editor) :ground
           (editor-csi-buffer editor) ""
           (editor-paste-buffer editor) nil)
     (when (zerop (length (editor-text editor)))
       (list (new-input-event :eof))))
    (t
     (case (editor-parser-state editor)
       (:ground
        (cond
          ((char= character #\Escape)
           (setf (editor-parser-state editor) :escape)
           nil)
          ((or (char= character #\Return)
               (char= character #\Newline))
           (list (new-input-event :enter (get-input-editor-text editor))))
          ((or (char= character #\Backspace)
               (= (char-code character) #x7f))
           (del-input-editor-before-cursor editor)
           nil)
          ((>= (char-code character) #x20)
           (set-input-editor-insert-character editor character)
           nil)))
       (:escape
        (if (char= character #\[)
            (setf (editor-parser-state editor) :csi
                  (editor-csi-buffer editor) "")
            (setf (editor-parser-state editor) :ground))
        nil)
       (:csi
        (let ((code (char-code character)))
          (if (and (<= (char-code #\@) code) (<= code (char-code #\~)))
              (progn
                (set-input-editor-csi editor character)
                (unless (eq (editor-parser-state editor) :paste)
                  (setf (editor-parser-state editor) :ground)))
              (setf (editor-csi-buffer editor)
                    (concatenate 'string
                                 (editor-csi-buffer editor)
                                 (string character)))))
        nil)
       (:paste
        (if (char= character #\Escape)
            (setf (editor-parser-state editor) :paste-escape)
            (push character (editor-paste-buffer editor)))
        nil)
       (:paste-escape
        (if (char= character #\[)
            (setf (editor-parser-state editor) :paste-csi
                  (editor-csi-buffer editor) "")
            (progn
              (set-input-editor-paste-text editor
                                 (concatenate 'string
                                              (string #\Escape)
                                              (string character)))
              (setf (editor-parser-state editor) :paste)))
        nil)
      (:paste-csi
        (set-input-editor-paste-character editor character))))))

(defun set-input-editor-bytes (editor bytes)
  "Consume octet BYTES and return any input events.

The editor keeps incomplete UTF-8 and escape sequences between calls."
  (check-type editor input-editor)
  (check-type bytes vector)
  (let ((pending (editor-decode-pending editor))
        (events nil)
        (segment-start 0)
        (length (length bytes)))
    (labels ((process-text (text)
               (loop for character across text
                     do (dolist (event (set-input-editor-character editor character))
                          (push event events))))
             (process-segment (start end)
               (when (< start end)
                 (let ((segment (if (and (zerop start)
                                         (= end length))
                                    bytes
                                    (subseq bytes start end))))
                   (multiple-value-bind (text new-pending)
                       (get-utf8-chunk segment pending)
                     (setf pending new-pending)
                     (process-text text))))))
      (loop for index below length
            for byte = (aref bytes index)
            when (or (= byte 3) (= byte 4))
              do (process-segment segment-start index)
                 ;; Control bytes cannot continue a UTF-8 sequence.
                 (setf pending nil
                       segment-start (1+ index))
                 (dolist (event (set-input-editor-character editor (code-char byte)))
                   (push event events)))
      (process-segment segment-start length)
      (setf (editor-decode-pending editor) pending)
      (nreverse events))))

(defun set-input-editor-paste (editor text)
  "Insert pasted TEXT without treating newlines as submissions."
  (check-type editor input-editor)
  (check-type text string)
  (set-input-editor-insert-text editor text))

(defun get-input-language (text)
  "Return the language selected by TEXT's first character."
  (if (and (plusp (length text))
           (char= (char text 0) (code-char 40)))
      :lisp
      :shell))

(defun get-whitespace-character-p (character)
  "Return true when CHARACTER separates input words."
  (member character '(#\Space #\Tab #\Newline #\Return) :test #'char=))

(defun get-heredoc-delimiter (line start)
  "Read a here-document delimiter from LINE at START."
  (let ((index start)
        (value nil)
        (quote nil)
        (escaped-p nil)
        (length (length line)))
    (loop while (< index length)
          for character = (char line index)
          do (cond
               (escaped-p
                (push character value)
                (setf escaped-p nil)
                (incf index))
               (quote
                (if (char= character quote)
                    (setf quote nil)
                    (push character value))
                (incf index))
               ((char= character #\\)
                (setf escaped-p t)
                (incf index))
               ((member character '(#\' #\" #\`) :test #'char=)
                (setf quote character)
                (incf index))
               ((or (get-whitespace-character-p character)
                    (member character '(#\; #\| #\& #\< #\>)
                            :test #'char=))
                (return))
               (t
                (push character value)
                (incf index))))
    (when (and value (null quote) (not escaped-p))
      (values (coerce (nreverse value) 'string) index))))

(defun get-heredoc-delimiters (line quote escaped-p)
  "Find here-document declarations in one shell source LINE."
  (let ((index 0)
        (length (length line))
        (word-start-p t)
        (comment-p nil)
        (delimiters nil)
        (malformed-p nil))
    (loop while (< index length)
          for character = (char line index)
          do (cond
               (comment-p
                (setf index length))
               (escaped-p
                (setf escaped-p nil
                      word-start-p nil)
                (incf index))
               (quote
                (if (char= character quote)
                    (setf quote nil)
                    (when (and (char= quote #\")
                               (char= character #\\))
                      (setf escaped-p t)))
                (incf index))
               ((char= character #\\)
                (setf escaped-p t
                      word-start-p nil)
                (incf index))
               ((member character '(#\' #\" #\`) :test #'char=)
                (setf quote character
                      word-start-p nil)
                (incf index))
               ((and (char= character #\#) word-start-p)
                (setf comment-p t
                      index length))
               ((get-whitespace-character-p character)
                (setf word-start-p t)
                (incf index))
               ((and (char= character #\<)
                     (< (1+ index) length)
                     (char= (char line (1+ index)) #\<)
                     (or (= (+ index 2) length)
                         (not (char= (char line (+ index 2)) #\<))))
                (let ((delimiter-start (+ index 2))
                      (strip-tabs-p nil))
                  (when (and (< delimiter-start length)
                             (char= (char line delimiter-start) #\-))
                    (setf strip-tabs-p t)
                    (incf delimiter-start))
                  (loop while (and (< delimiter-start length)
                                   (member (char line delimiter-start)
                                           '(#\Space #\Tab)
                                           :test #'char=))
                        do (incf delimiter-start))
                  (multiple-value-bind (delimiter next-index)
                      (get-heredoc-delimiter line delimiter-start)
                    (if delimiter
                        (progn
                          (push (cons delimiter strip-tabs-p) delimiters)
                          (setf index next-index
                                word-start-p nil))
                        (setf malformed-p t
                              index length)))))
               (t
                (setf word-start-p nil)
                (incf index))))
    (values (nreverse delimiters) quote escaped-p malformed-p)))

(defun get-shell-heredoc-code (text)
  "Return shell TEXT without here-document bodies.

The first value reports whether a here-document remains unfinished."
  (let ((position 0)
        (length (length text))
        (quote nil)
        (escaped-p nil)
        (pending nil))
    (let ((code
            (with-output-to-string (output)
              (loop while (< position length)
                    do (let* ((line-end
                                (or (position #\Newline text :start position)
                                    length))
                              (next-position (if (< line-end length)
                                                 (1+ line-end)
                                                 line-end))
                              (newline-p (< line-end length))
                              (raw-line (subseq text position line-end))
                              (line raw-line))
                         (when (and (plusp (length line))
                                    (char= (char line (1- (length line)))
                                           #\Return))
                           (setf line (subseq line 0 (1- (length line)))))
                         (if pending
                             (let ((content (if (cdr (first pending))
                                                (string-left-trim
                                                 (string #\Tab)
                                                 line)
                                                line)))
                               (when (string= content (car (first pending)))
                                 (setf pending (rest pending)))
                               ;; Hide here-document content from syntax checks.
                               (when newline-p
                                 (write-char #\Newline output)))
                             (multiple-value-bind
                                   (delimiters new-quote new-escaped-p malformed-p)
                                 (get-heredoc-delimiters
                                  line
                                  quote
                                  escaped-p)
                               (when malformed-p
                                 (return-from get-shell-heredoc-code
                                   (values t nil)))
                               (setf quote new-quote
                                     escaped-p new-escaped-p
                                     pending (nconc pending delimiters))
                               (write-string raw-line output)
                               (when newline-p
                                 (write-char #\Newline output))))
                         (setf position next-position))))))
      (values (not (null pending)) code))))

(defun get-shell-compound-incomplete-p (tokens)
  "Return true when TOKENS leave a common shell compound open."
  (let ((stack nil))
    (dolist (entry (reverse tokens))
      (when (rest entry)
        (let ((word (string-downcase (first entry))))
          (cond
            ((string= word "if") (push :if stack))
            ((and (string= word "fi") (eq (first stack) :if))
             (pop stack))
            ((member word '("for" "while" "until" "select") :test #'string=)
             (push :loop stack))
            ((and (string= word "done") (eq (first stack) :loop))
             (pop stack))
            ((string= word "case") (push :case stack))
            ((and (string= word "esac") (eq (first stack) :case))
             (pop stack))))))
    (or stack
        (let ((last-entry (first tokens)))
          (and last-entry
               (rest last-entry)
               (member (string-downcase (first last-entry))
                       '("then" "do" "else" "elif" "in")
                       :test #'string=))))))

(defun get-shell-syntax-completeness (text)
  "Return :COMPLETE or :INCOMPLETE for shell syntax TEXT."
  (let ((quote nil)
        (escaped-p nil)
        (comment-p nil)
        (delimiter-stack nil)
        (token nil)
        (tokens nil)
        (trailing-operator nil)
        (word-start-p t)
        (command-position-p t)
        (token-command-position-p nil))
    (labels ((set-token-finished ()
               (when token
                 (push (cons (coerce (nreverse token) 'string)
                             token-command-position-p)
                       tokens)
                 (setf token nil
                       token-command-position-p nil))
               (setf word-start-p t))
             (set-token-start ()
               (when (null token)
                 (setf token-command-position-p command-position-p))
               (setf command-position-p nil
                     word-start-p nil))
             (set-token-character (character)
               (set-token-start)
               (push character token)
               (setf trailing-operator nil)))
      ;; Track only syntax that commonly needs another input line.
      (loop for character across text
            do (cond
                 (comment-p
                  (when (or (char= character #\Newline)
                            (char= character #\Return))
                    (setf comment-p nil
                          word-start-p t
                          command-position-p t)))
                 (escaped-p
                  (set-token-character character)
                  (setf escaped-p nil))
                 (quote
                  (case quote
                    (:single
                     (if (char= character #\')
                         (setf quote nil)
                         (set-token-character character)))
                    (:double
                     (cond
                       ((char= character #\") (setf quote nil))
                       ((char= character #\\) (setf escaped-p t))
                       (t (set-token-character character))))
                    (:backtick
                     (if (char= character #\`)
                         (setf quote nil)
                         (set-token-character character)))))
                 ((char= character #\\)
                  (set-token-start)
                  (setf escaped-p t
                        trailing-operator nil))
                 ((char= character #\')
                  (set-token-start)
                  (setf quote :single
                        trailing-operator nil))
                 ((char= character #\")
                  (set-token-start)
                  (setf quote :double
                        trailing-operator nil))
                 ((char= character #\`)
                  (set-token-start)
                  (setf quote :backtick
                        trailing-operator nil))
                 ((and (char= character #\#) word-start-p)
                  (setf comment-p t))
                 ((get-whitespace-character-p character)
                  (set-token-finished)
                  (setf word-start-p t)
                  (when (or (char= character #\Newline)
                            (char= character #\Return))
                    (setf command-position-p t)))
                 ((member character '(#\( #\{) :test #'char=)
                  (set-token-finished)
                  (push character delimiter-stack)
                  (setf command-position-p t
                        trailing-operator nil))
                 ((member character '(#\) #\}) :test #'char=)
                  (set-token-finished)
                  (when (and delimiter-stack
                             (char= character
                                    (case (first delimiter-stack)
                                      (#\( #\))
                                      (#\{ #\})
                                      (otherwise #\Null))))
                    (pop delimiter-stack))
                  (setf trailing-operator nil))
                 ((char= character #\;)
                  (set-token-finished)
                  (setf command-position-p t
                        trailing-operator nil))
                 ((char= character #\|)
                  (set-token-finished)
                  (setf command-position-p t
                        trailing-operator
                        (if (string= trailing-operator "|")
                            "||"
                            "|")))
                 ((char= character #\&)
                  (set-token-finished)
                  (setf command-position-p t
                        trailing-operator
                        (cond
                          ((string= trailing-operator "|") "|&")
                          ((string= trailing-operator "&") "&&")
                          (t "&"))))
                 (t (set-token-character character))))
      (set-token-finished))
    (if (or quote
            escaped-p
            delimiter-stack
            (member trailing-operator '("|" "||" "&&" "|&")
                    :test #'string=)
            (get-shell-compound-incomplete-p tokens))
        :incomplete
        :complete)))

(defun get-shell-input-completeness (text)
  "Return :COMPLETE or :INCOMPLETE for shell TEXT."
  (multiple-value-bind (heredoc-incomplete-p shell-text)
      (get-shell-heredoc-code text)
    (if heredoc-incomplete-p
        :incomplete
        (get-shell-syntax-completeness shell-text))))

(defparameter +input-reader-eof+ (gensym "INPUT-READER-EOF"))

(defun get-lisp-input-completeness (text)
  "Return the completeness of one Lispore Lisp candidate."
  (handler-case
      (let ((*read-eval* nil))
        (multiple-value-bind (form position)
            (read-from-string text nil +input-reader-eof+)
          (cond
            ((eq form +input-reader-eof+) :incomplete)
            ((every #'get-whitespace-character-p
                    (subseq text position))
             :complete)
            (t (get-shell-input-completeness text)))))
    (end-of-file () :incomplete)
    (error () :error)))

(defun get-input-completeness (text)
  "Return :COMPLETE, :INCOMPLETE, or :ERROR for input TEXT."
  (check-type text string)
  (if (eq :lisp (get-input-language text))
      (get-lisp-input-completeness text)
      (get-shell-input-completeness text)))
