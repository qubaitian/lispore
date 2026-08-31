(in-package #:lispore.terminal)

(defconstant +ascii-bell-code+ 7)
(defparameter *default-status-line-text* " lispore | shell ")

(defstruct (screen-cell
            (:constructor new-screen-cell (character style))
            (:copier nil))
  (character #\Space :type character)
  (style nil :type list))

(defun get-screen-cell-copy (cell)
  "Return an independent copy of CELL."
  (new-screen-cell (screen-cell-character cell)
                   (copy-list (screen-cell-style cell))))

(defclass terminal-emulator ()
  ((width
    :initarg :width
    :accessor terminal-width)
   (height
    :initarg :height
    :accessor terminal-height)
   ;; The bottom status line stays outside the shell content area.
   (content-height
    :initarg :content-height
    :accessor terminal-content-height)
   (cells
    :initarg :cells
    :accessor terminal-cells)
   (status-line
    :initform nil
    :accessor terminal-status-line)
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

(defun new-terminal-row (width)
  (make-array width
              :initial-contents
              (loop repeat width
                    collect (new-screen-cell #\Space nil))))

(defun new-terminal-screen (width height)
  (make-array height
              :initial-contents
              (loop repeat height
                    collect (new-terminal-row width))))

(defun new-terminal-emulator (&key (width 80)
                                    (height 24)
                                    (content-height height))
  "Create a terminal emulator with WIDTH columns and HEIGHT rows."
  (check-type width (integer 1))
  (check-type height (integer 1))
  (check-type content-height (integer 1))
  (unless (<= content-height height)
    (error "The content height cannot exceed the terminal height."))
  (make-instance 'terminal-emulator
                 :width width
                 :height height
                 :content-height content-height
                 :cells (new-terminal-screen width height)))

(defun get-terminal-size (terminal)
  "Return the emulator size as width and height values."
  (values (terminal-width terminal)
          (terminal-height terminal)))

(defun get-terminal-copy (terminal)
  "Return an independent copy of TERMINAL's retained screen state."
  (let ((copy (new-terminal-emulator
               :width (terminal-width terminal)
               :height (terminal-height terminal)
               :content-height (terminal-content-height terminal))))
    (setf (terminal-cells copy)
          (make-array
           (length (terminal-cells terminal))
           :initial-contents
           (loop for row across (terminal-cells terminal)
                 collect (make-array
                          (length row)
                          :initial-contents
                          (loop for cell across row
                                collect (let ((cell-copy (get-screen-cell-copy cell)))
                                          (setf (screen-cell-style cell-copy)
                                                (copy-list (screen-cell-style cell)))
                                          cell-copy)))))
          (terminal-status-line copy)
          (and (terminal-status-line terminal)
               (copy-seq (terminal-status-line terminal)))
          (cursor-row copy) (cursor-row terminal)
          (cursor-column copy) (cursor-column terminal)
          (current-style copy) (copy-list (current-style terminal))
          (saved-row copy) (saved-row terminal)
          (saved-column copy) (saved-column terminal)
          (saved-style copy) (copy-list (saved-style terminal))
          (parser-state copy) (parser-state terminal)
          (csi-buffer copy) (copy-seq (csi-buffer terminal))
          (osc-escape-p copy) (osc-escape-p terminal))
    copy))

(defun set-terminal-status-line-drawing (terminal)
  "Draw TERMINAL's status line in its reserved bottom row."
  (let* ((row (aref (terminal-cells terminal)
                    (1- (terminal-height terminal))))
         (text (terminal-status-line terminal))
         (width (terminal-width terminal)))
    (loop for column below width
          for character = (if (and text (< column (length text)))
                              (char text column)
                              #\Space)
          do (setf (screen-cell-character (aref row column)) character
                   ;; ANSI 30 is black foreground. ANSI 42 is green background.
                   (screen-cell-style (aref row column)) (list 30 42)))))

(defun set-terminal-status-line (terminal text)
  "Reserve TERMINAL's bottom row and display TEXT there."
  (check-type text string)
  (setf (terminal-status-line terminal) text
        (terminal-content-height terminal)
        (max 1 (1- (terminal-height terminal))))
  (set-terminal-status-line-drawing terminal)
  (setf (cursor-row terminal)
        (min (cursor-row terminal) (1- (terminal-content-height terminal))))
  terminal)

(defun set-terminal-size (terminal width height)
  "Resize TERMINAL and preserve its visible content where possible."
  (check-type width (integer 1))
  (check-type height (integer 1))
  (let* ((old-width (terminal-width terminal))
         (old-content-height (terminal-content-height terminal))
         (content-height (if (terminal-status-line terminal)
                             (max 1 (1- height))
                             height))
         (old-cells (terminal-cells terminal))
         (cells (new-terminal-screen width height)))
    (loop for row below (min old-content-height content-height)
          for old-row = (aref old-cells row)
          for new-row = (aref cells row)
          do (loop for column below (min old-width width)
                   do (setf (aref new-row column)
                            (get-screen-cell-copy (aref old-row column)))))
    (setf (terminal-width terminal) width
          (terminal-height terminal) height
          (terminal-content-height terminal) content-height
          (terminal-cells terminal) cells
          (cursor-row terminal)
          (min (cursor-row terminal) (1- content-height))
          (cursor-column terminal)
          (min (cursor-column terminal) width)
          (saved-row terminal)
          (min (saved-row terminal) (1- content-height))
          (saved-column terminal)
          (min (saved-column terminal) width))
    (when (terminal-status-line terminal)
      (set-terminal-status-line-drawing terminal)))
  terminal)

(defun get-terminal-cell (terminal row column)
  "Return a copy of the cell at one-based ROW and COLUMN."
  (check-type row (integer 1))
  (check-type column (integer 1))
  (get-screen-cell-copy
   (aref (aref (terminal-cells terminal) (1- row)) (1- column))))

(defun get-terminal-screen-lines (terminal)
  "Return the terminal screen as full-width strings."
  (loop with width = (terminal-width terminal)
        for row across (terminal-cells terminal)
        collect (let ((line (make-string width)))
                  (loop for column below width
                        for cell = (aref row column)
                        do (setf (char line column)
                                 (screen-cell-character cell)))
                  line)))

(defun get-terminal-cursor-position (terminal)
  "Return the cursor position as one-based row and column values."
  (values (1+ (cursor-row terminal))
          (1+ (min (cursor-column terminal)
                   (1- (terminal-width terminal))))))

(defun get-clamped-value (value minimum maximum)
  (max minimum (min value maximum)))

(defun set-terminal-reset (terminal)
  (let ((status-line (terminal-status-line terminal)))
    (setf (terminal-cells terminal)
          (new-terminal-screen (terminal-width terminal)
                             (terminal-height terminal))
          (cursor-row terminal) 0
          (cursor-column terminal) 0
          (current-style terminal) nil
          (parser-state terminal) :ground
          (csi-buffer terminal) ""
          (osc-escape-p terminal) nil)
    ;; Shell reset sequences must not remove the frontend status line.
    (when status-line
      (set-terminal-status-line-drawing terminal)))
  terminal)

(defun set-terminal-scroll (terminal)
  (let ((cells (terminal-cells terminal))
        (height (terminal-content-height terminal)))
    (loop for row below (1- height)
          do (setf (aref cells row) (aref cells (1+ row))))
    (setf (aref cells (1- height))
          (new-terminal-row (terminal-width terminal)))))

(defun set-terminal-line-feed (terminal)
  (if (= (cursor-row terminal) (1- (terminal-content-height terminal)))
      (set-terminal-scroll terminal)
      (incf (cursor-row terminal))))

(defun set-terminal-character (terminal character)
  (when (>= (cursor-column terminal) (terminal-width terminal))
    (setf (cursor-column terminal) 0)
    (set-terminal-line-feed terminal))
  (let ((cell (aref (aref (terminal-cells terminal) (cursor-row terminal))
                    (cursor-column terminal))))
    (setf (screen-cell-character cell) character
          (screen-cell-style cell) (copy-list (current-style terminal))))
  (incf (cursor-column terminal)))

(defun set-terminal-cursor (terminal row column)
  (setf (cursor-row terminal)
        (get-clamped-value row 0 (1- (terminal-content-height terminal)))
        (cursor-column terminal)
        (get-clamped-value column 0 (terminal-width terminal))))

(defun del-terminal-cell (terminal row column)
  (setf (aref (aref (terminal-cells terminal) row) column)
        (new-screen-cell #\Space nil)))

(defun del-terminal-line (terminal mode)
  (let* ((row (cursor-row terminal))
         (start (if (= mode 1) 0 (cursor-column terminal)))
         (end (if (= mode 1)
                  (min (cursor-column terminal) (1- (terminal-width terminal)))
                  (1- (terminal-width terminal)))))
    (when (= mode 2)
      (setf start 0 end (1- (terminal-width terminal))))
    (loop for column from start to end
          do (del-terminal-cell terminal row column))))

(defun del-terminal-display (terminal mode)
  (let ((row (cursor-row terminal))
        (column (cursor-column terminal))
        (height (terminal-content-height terminal))
        (width (terminal-width terminal)))
    (cond
      ((= mode 2)
       (loop for screen-row below height
             do (loop for screen-column below width
                      do (del-terminal-cell terminal screen-row screen-column))))
      ((= mode 1)
       (loop for screen-row from 0 to row
             do (loop for screen-column below width
                      when (or (< screen-row row)
                               (<= screen-column column))
                        do (del-terminal-cell terminal screen-row screen-column))))
      (t
       (loop for screen-row from row below height
             do (loop for screen-column below width
                      when (or (> screen-row row)
                               (>= screen-column column))
                        do (del-terminal-cell terminal screen-row screen-column)))))))

(defun get-csi-default-parameter (parameters index default)
  (let ((value (nth index parameters)))
    (if (or (null value) (zerop value)) default value)))

(defun get-csi-parameters (buffer)
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

(defun get-style-kind (code)
  ;; ANSI SGR assigns these ranges to styles and colors.
  (cond
    ((member code '(1 2 3 4)) :decoration)
    ((or (<= 30 code 37) (<= 90 code 97)) :foreground)
    ((or (<= 40 code 47) (<= 100 code 107)) :background)
    (t nil)))

(defun set-terminal-style-code (terminal code)
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
                        (eq (get-style-kind item)
                            (if (= code 39) :foreground :background)))
                      (current-style terminal))))
    (t
     (let ((kind (get-style-kind code)))
       (when kind
         (setf (current-style terminal)
               (remove-if (lambda (item)
                            (eq (get-style-kind item) kind))
                          (current-style terminal))))
     (pushnew code (current-style terminal) :test #'eql)))))

(defun set-terminal-sgr (terminal parameters)
  (dolist (code (or parameters '(0)))
    (set-terminal-style-code terminal (or code 0))))

(defun set-terminal-csi (terminal final)
  (multiple-value-bind (parameters private)
      (get-csi-parameters (csi-buffer terminal))
    (declare (ignore private))
    (case final
      (#\A (set-terminal-cursor terminal
                        (- (cursor-row terminal)
                           (get-csi-default-parameter parameters 0 1))
                        (cursor-column terminal)))
      (#\B (set-terminal-cursor terminal
                        (+ (cursor-row terminal)
                           (get-csi-default-parameter parameters 0 1))
                        (cursor-column terminal)))
      (#\C (set-terminal-cursor terminal
                        (cursor-row terminal)
                        (+ (cursor-column terminal)
                           (get-csi-default-parameter parameters 0 1))))
      (#\D (set-terminal-cursor terminal
                        (cursor-row terminal)
                        (- (cursor-column terminal)
                           (get-csi-default-parameter parameters 0 1))))
      (#\E (set-terminal-cursor terminal
                        (+ (cursor-row terminal)
                           (get-csi-default-parameter parameters 0 1))
                        0))
      (#\F (set-terminal-cursor terminal
                        (- (cursor-row terminal)
                           (get-csi-default-parameter parameters 0 1))
                        0))
      ((#\G #\`) (set-terminal-cursor terminal
                   (cursor-row terminal)
                   (1- (get-csi-default-parameter parameters 0 1))))
      (#\d (set-terminal-cursor terminal
                        (1- (get-csi-default-parameter parameters 0 1))
                        (cursor-column terminal)))
      ((#\H #\f) (set-terminal-cursor terminal
                   (1- (get-csi-default-parameter parameters 0 1))
                   (1- (get-csi-default-parameter parameters 1 1))))
      (#\J (del-terminal-display terminal (or (first parameters) 0)))
      (#\K (del-terminal-line terminal (or (first parameters) 0)))
      (#\m (set-terminal-sgr terminal parameters))
      (#\s (setf (saved-row terminal) (cursor-row terminal)
                  (saved-column terminal) (cursor-column terminal)
                  (saved-style terminal) (copy-list (current-style terminal))))
      (#\u (set-terminal-cursor terminal (saved-row terminal) (saved-column terminal))
            (setf (current-style terminal) (copy-list (saved-style terminal))))
      ;; CSI c queries device attributes. It does not reset the screen.
      (#\c nil))))

(defun set-terminal-escape-character (terminal character)
  (case character
    (#\[ (setf (parser-state terminal) :csi
                (csi-buffer terminal) ""))
    (#\] (setf (parser-state terminal) :osc
                (osc-escape-p terminal) nil))
    (#\7 (setf (saved-row terminal) (cursor-row terminal)
                (saved-column terminal) (cursor-column terminal)
                (saved-style terminal) (copy-list (current-style terminal))
                (parser-state terminal) :ground))
    (#\8 (set-terminal-cursor terminal (saved-row terminal) (saved-column terminal))
          (setf (current-style terminal) (copy-list (saved-style terminal))
                (parser-state terminal) :ground))
    (#\c (set-terminal-reset terminal))
    (#\D (set-terminal-line-feed terminal)
          (setf (parser-state terminal) :ground))
    (#\E (setf (cursor-column terminal) 0)
          (set-terminal-line-feed terminal)
          (setf (parser-state terminal) :ground))
    (#\M (if (plusp (cursor-row terminal))
              (decf (cursor-row terminal)))
          (setf (parser-state terminal) :ground))
    ((#\( #\)) (setf (parser-state terminal) :escape-intermediate))
    (otherwise (setf (parser-state terminal) :ground))))

(defun set-terminal-csi-character (terminal character)
  (let ((code (char-code character)))
    (cond
      ((and (<= #x40 code) (<= code #x7e))
       (set-terminal-csi terminal character)
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

(defun set-terminal-input-character (terminal character)
  (case (parser-state terminal)
    (:ground
     (case character
       (#\Escape (setf (parser-state terminal) :escape))
       (#\Newline (set-terminal-line-feed terminal))
       (#\Return (setf (cursor-column terminal) 0))
       (#\Backspace (setf (cursor-column terminal)
                           (max 0 (1- (cursor-column terminal)))))
       (#\Tab (setf (cursor-column terminal)
                     (min (terminal-width terminal)
                          (* 8 (1+ (floor (cursor-column terminal) 8))))))
       (otherwise
        (unless (= (char-code character) +ascii-bell-code+)
          (when (>= (char-code character) #x20)
            (set-terminal-character terminal character))))))
    (:escape (set-terminal-escape-character terminal character))
    (:escape-intermediate (setf (parser-state terminal) :ground))
    (:csi (set-terminal-csi-character terminal character))
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

(defun set-terminal-input (terminal text)
  "Feed decoded UTF-8 terminal text into TERMINAL."
  (check-type text string)
  (loop for character across text
        do (set-terminal-input-character terminal character))
  terminal)

(defun set-terminal-style (stream style)
  (write-string (format nil "~C[~{~A~^;~}m" #\Escape style) stream))

(defun get-terminal-render (terminal)
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
                              (set-terminal-style stream cell-style)
                              (set-terminal-style stream '(0)))
                          (setf style cell-style))
                        (write-char (screen-cell-character cell) stream))
               (when style
                 (set-terminal-style stream '(0)))
               (when (< row-index (1- (terminal-height terminal)))
                 (write-string (format nil "~C~C" #\Return #\Newline)
                               stream))))
    (format stream "~C[~D;~DH"
            #\Escape
            (1+ (cursor-row terminal))
            (1+ (min (cursor-column terminal)
                     (1- (terminal-width terminal)))))))
