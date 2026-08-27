(in-package #:lispore.terminal)

(defconstant +ascii-bell-code+ 7)

(defstruct (screen-cell
            (:constructor make-screen-cell (character style)))
  (character #\Space :type character)
  (style nil :type list))

(defclass terminal-emulator ()
  ((width
    :initarg :width
    :reader terminal-width)
   (height
    :initarg :height
    :reader terminal-height)
   (cells
    :initarg :cells
    :accessor terminal-cells)
   (cursor-row
    :initform 0
    :accessor cursor-row)
   (cursor-column
    :initform 0
    :accessor cursor-column)
   (current-style
    :initform nil
    :accessor current-style)
   (saved-row
    :initform 0
    :accessor saved-row)
   (saved-column
    :initform 0
    :accessor saved-column)
   (saved-style
    :initform nil
    :accessor saved-style)
   (parser-state
    :initform :ground
    :accessor parser-state)
   (csi-buffer
    :initform ""
    :accessor csi-buffer)
   (osc-escape-p
    :initform nil
    :accessor osc-escape-p)))

(defun make-blank-row (width)
  (make-array width
              :initial-contents
              (loop repeat width
                    collect (make-screen-cell #\Space nil))))

(defun make-blank-screen (width height)
  (make-array height
              :initial-contents
              (loop repeat height
                    collect (make-blank-row width))))

(defun make-terminal-emulator (&key (width 80) (height 24))
  "Create a terminal emulator with WIDTH columns and HEIGHT rows."
  (check-type width (integer 1))
  (check-type height (integer 1))
  (make-instance 'terminal-emulator
                 :width width
                 :height height
                 :cells (make-blank-screen width height)))

(defun terminal-size (terminal)
  "Return the emulator size as width and height values."
  (values (terminal-width terminal)
          (terminal-height terminal)))

(defun cell-at (terminal row column)
  "Return a copy of the cell at one-based ROW and COLUMN."
  (check-type row (integer 1))
  (check-type column (integer 1))
  (copy-screen-cell
   (aref (aref (terminal-cells terminal) (1- row)) (1- column))))

(defun screen-lines (terminal)
  "Return the terminal screen as full-width strings."
  (loop with width = (terminal-width terminal)
        for row across (terminal-cells terminal)
        collect (let ((line (make-string width)))
                  (loop for column below width
                        for cell = (aref row column)
                        do (setf (char line column)
                                 (screen-cell-character cell)))
                  line)))

(defun cursor-position (terminal)
  "Return the cursor position as one-based row and column values."
  (values (1+ (cursor-row terminal))
          (1+ (min (cursor-column terminal)
                   (1- (terminal-width terminal))))))

(defun clamp (value minimum maximum)
  (max minimum (min value maximum)))

(defun reset-terminal (terminal)
  (setf (terminal-cells terminal)
        (make-blank-screen (terminal-width terminal)
                           (terminal-height terminal))
        (cursor-row terminal) 0
        (cursor-column terminal) 0
        (current-style terminal) nil
        (parser-state terminal) :ground
        (csi-buffer terminal) ""
        (osc-escape-p terminal) nil)
  terminal)

(defun scroll-up (terminal)
  (let ((cells (terminal-cells terminal))
        (height (terminal-height terminal)))
    (loop for row below (1- height)
          do (setf (aref cells row) (aref cells (1+ row))))
    (setf (aref cells (1- height))
          (make-blank-row (terminal-width terminal)))))

(defun line-feed (terminal)
  (if (= (cursor-row terminal) (1- (terminal-height terminal)))
      (scroll-up terminal)
      (incf (cursor-row terminal))))

(defun put-character (terminal character)
  (when (>= (cursor-column terminal) (terminal-width terminal))
    (setf (cursor-column terminal) 0)
    (line-feed terminal))
  (let ((cell (aref (aref (terminal-cells terminal) (cursor-row terminal))
                    (cursor-column terminal))))
    (setf (screen-cell-character cell) character
          (screen-cell-style cell) (copy-list (current-style terminal))))
  (incf (cursor-column terminal)))

(defun move-cursor (terminal row column)
  (setf (cursor-row terminal)
        (clamp row 0 (1- (terminal-height terminal)))
        (cursor-column terminal)
        (clamp column 0 (terminal-width terminal))))

(defun erase-cell (terminal row column)
  (setf (aref (aref (terminal-cells terminal) row) column)
        (make-screen-cell #\Space nil)))

(defun erase-line (terminal mode)
  (let* ((row (cursor-row terminal))
         (start (if (= mode 1) 0 (cursor-column terminal)))
         (end (if (= mode 1)
                  (min (cursor-column terminal) (1- (terminal-width terminal)))
                  (1- (terminal-width terminal)))))
    (when (= mode 2)
      (setf start 0 end (1- (terminal-width terminal))))
    (loop for column from start to end
          do (erase-cell terminal row column))))

(defun erase-display (terminal mode)
  (let ((row (cursor-row terminal))
        (column (cursor-column terminal))
        (height (terminal-height terminal))
        (width (terminal-width terminal)))
    (cond
      ((= mode 2)
       (loop for screen-row below height
             do (loop for screen-column below width
                      do (erase-cell terminal screen-row screen-column))))
      ((= mode 1)
       (loop for screen-row from 0 to row
             do (loop for screen-column below width
                      when (or (< screen-row row)
                               (<= screen-column column))
                        do (erase-cell terminal screen-row screen-column))))
      (t
       (loop for screen-row from row below height
             do (loop for screen-column below width
                      when (or (> screen-row row)
                               (>= screen-column column))
                        do (erase-cell terminal screen-row screen-column)))))))

(defun default-parameter (parameters index default)
  (let ((value (nth index parameters)))
    (if (or (null value) (zerop value)) default value)))

(defun parse-csi-parameters (buffer)
  (let* ((private (and (plusp (length buffer))
                       (member (char buffer 0) '(#\? #\> #\< #\=))))
         (start (if private 1 0))
         (values nil)
         (field-start start))
    (loop for index from start to (length buffer)
          when (or (= index (length buffer))
                   (char= (char buffer index) #\;))
            do (push (unless (= field-start index)
                       (parse-integer buffer :start field-start :end index))
                     values)
               (setf field-start (1+ index)))
    (values (nreverse values) private)))

(defun style-kind (code)
  ;; ANSI SGR assigns these ranges to styles and colors.
  (cond
    ((member code '(1 2 3 4)) :decoration)
    ((or (<= 30 code 37) (<= 90 code 97)) :foreground)
    ((or (<= 40 code 47) (<= 100 code 107)) :background)
    (t nil)))

(defun set-style-code (terminal code)
  (cond
    ((= code 0) (setf (current-style terminal) nil))
    ((member code '(22 23 24))
     (setf (current-style terminal)
           (remove-if (lambda (item)
                        (member item (case code
                                       (22 '(1 2))
                                       (23 '(3))
                                       (24 '(4)))))
                      (current-style terminal))))
    ((member code '(39 49))
     (setf (current-style terminal)
           (remove-if (lambda (item)
                        (eq (style-kind item)
                            (if (= code 39) :foreground :background)))
                      (current-style terminal))))
    (t
     (let ((kind (style-kind code)))
       (when kind
         (setf (current-style terminal)
               (remove-if (lambda (item)
                            (eq (style-kind item) kind))
                          (current-style terminal))))
     (pushnew code (current-style terminal) :test #'eql)))))

(defun handle-sgr (terminal parameters)
  (dolist (code (or parameters '(0)))
    (set-style-code terminal (or code 0))))

(defun handle-csi (terminal final)
  (multiple-value-bind (parameters private)
      (parse-csi-parameters (csi-buffer terminal))
    (declare (ignore private))
    (case final
      (#\A (move-cursor terminal
                        (- (cursor-row terminal)
                           (default-parameter parameters 0 1))
                        (cursor-column terminal)))
      (#\B (move-cursor terminal
                        (+ (cursor-row terminal)
                           (default-parameter parameters 0 1))
                        (cursor-column terminal)))
      (#\C (move-cursor terminal
                        (cursor-row terminal)
                        (+ (cursor-column terminal)
                           (default-parameter parameters 0 1))))
      (#\D (move-cursor terminal
                        (cursor-row terminal)
                        (- (cursor-column terminal)
                           (default-parameter parameters 0 1))))
      (#\E (move-cursor terminal
                        (+ (cursor-row terminal)
                           (default-parameter parameters 0 1))
                        0))
      (#\F (move-cursor terminal
                        (- (cursor-row terminal)
                           (default-parameter parameters 0 1))
                        0))
      ((#\G #\`) (move-cursor terminal
                   (cursor-row terminal)
                   (1- (default-parameter parameters 0 1))))
      (#\d (move-cursor terminal
                        (1- (default-parameter parameters 0 1))
                        (cursor-column terminal)))
      ((#\H #\f) (move-cursor terminal
                   (1- (default-parameter parameters 0 1))
                   (1- (default-parameter parameters 1 1))))
      (#\J (erase-display terminal (or (first parameters) 0)))
      (#\K (erase-line terminal (or (first parameters) 0)))
      (#\m (handle-sgr terminal parameters))
      (#\s (setf (saved-row terminal) (cursor-row terminal)
                  (saved-column terminal) (cursor-column terminal)
                  (saved-style terminal) (copy-list (current-style terminal))))
      (#\u (move-cursor terminal (saved-row terminal) (saved-column terminal))
            (setf (current-style terminal) (copy-list (saved-style terminal))))
      ;; CSI c queries device attributes. It does not reset the screen.
      (#\c nil))))

(defun feed-escape-character (terminal character)
  (case character
    (#\[ (setf (parser-state terminal) :csi
                (csi-buffer terminal) ""))
    (#\] (setf (parser-state terminal) :osc
                (osc-escape-p terminal) nil))
    (#\7 (setf (saved-row terminal) (cursor-row terminal)
                (saved-column terminal) (cursor-column terminal)
                (saved-style terminal) (copy-list (current-style terminal))
                (parser-state terminal) :ground))
    (#\8 (move-cursor terminal (saved-row terminal) (saved-column terminal))
          (setf (current-style terminal) (copy-list (saved-style terminal))
                (parser-state terminal) :ground))
    (#\c (reset-terminal terminal))
    (#\D (line-feed terminal)
          (setf (parser-state terminal) :ground))
    (#\E (setf (cursor-column terminal) 0)
          (line-feed terminal)
          (setf (parser-state terminal) :ground))
    (#\M (if (plusp (cursor-row terminal))
              (decf (cursor-row terminal)))
          (setf (parser-state terminal) :ground))
    ((#\( #\)) (setf (parser-state terminal) :escape-intermediate))
    (otherwise (setf (parser-state terminal) :ground))))

(defun feed-csi-character (terminal character)
  (let ((code (char-code character)))
    (cond
      ((and (<= #x40 code) (<= code #x7e))
       (handle-csi terminal character)
       (setf (parser-state terminal) :ground
             (csi-buffer terminal) ""))
      ((or (<= #x30 code #x3f)
           (and (zerop (length (csi-buffer terminal)))
                (member character '(#\? #\> #\< #\=))))
       (setf (csi-buffer terminal)
             (concatenate 'string (csi-buffer terminal)
                          (string character))))
      ((<= #x20 code #x2f) nil)
      (t (setf (parser-state terminal) :ground
               (csi-buffer terminal) "")))))

(defun feed-terminal-character (terminal character)
  (case (parser-state terminal)
    (:ground
     (case character
       (#\Escape (setf (parser-state terminal) :escape))
       (#\Newline (line-feed terminal))
       (#\Return (setf (cursor-column terminal) 0))
       (#\Backspace (setf (cursor-column terminal)
                           (max 0 (1- (cursor-column terminal)))))
       (#\Tab (setf (cursor-column terminal)
                     (min (terminal-width terminal)
                          (* 8 (1+ (floor (cursor-column terminal) 8))))))
       (otherwise
        (unless (= (char-code character) +ascii-bell-code+)
          (when (>= (char-code character) #x20)
            (put-character terminal character))))))
    (:escape (feed-escape-character terminal character))
    (:escape-intermediate (setf (parser-state terminal) :ground))
    (:csi (feed-csi-character terminal character))
    (:osc
     (cond
       ;; SBCL names Unicode U+1F514 "Bell".
       ((= (char-code character) +ascii-bell-code+)
        (setf (parser-state terminal) :ground))
       ((char= character #\Escape)
        (setf (parser-state terminal) :osc-escape))))
    (:osc-escape
     (if (char= character #\\)
         (setf (parser-state terminal) :ground)
         (setf (parser-state terminal) :osc)))))

(defun feed-terminal (terminal text)
  "Feed decoded UTF-8 terminal text into TERMINAL."
  (check-type text string)
  (loop for character across text
        do (feed-terminal-character terminal character))
  terminal)

(defun write-style (stream style)
  (write-string (format nil "~C[~{~A~^;~}m" #\Escape style) stream))

(defun render-terminal (terminal)
  "Return the screen as ANSI text for the current terminal."
  (with-output-to-string (stream)
    (write-string (format nil "~C[H~C[2J" #\Escape #\Escape) stream)
    (loop for row across (terminal-cells terminal)
          for row-index below (terminal-height terminal)
          do (let ((style nil))
               (loop for cell across row
                     for cell-style = (screen-cell-style cell)
                     do (unless (equal style cell-style)
                          (if cell-style
                              (write-style stream cell-style)
                              (write-style stream '(0)))
                          (setf style cell-style))
                        (write-char (screen-cell-character cell) stream))
               (when style
                 (write-style stream '(0)))
               (when (< row-index (1- (terminal-height terminal)))
                 (write-string (format nil "~C~C" #\Return #\Newline)
                               stream))))
    (format stream "~C[~D;~DH"
            #\Escape
            (1+ (cursor-row terminal))
            (1+ (min (cursor-column terminal)
                     (1- (terminal-width terminal)))))))
