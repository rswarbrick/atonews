(in-package :atonews)

(defclass string-pointer ()
  ((str :initarg :str :reader str)
   (pos :initarg :pos :accessor pos))
  (:documentation "A bundled string-and-position."))

(defgeneric make-string-pointer (x &optional pos)
  (:documentation "Make a STRING-POINTER, clothing X."))

(defmethod make-string-pointer ((str string) &optional (pos 0))
  (make-instance 'string-pointer :str str :pos pos))

(defgeneric seek (string-pointer position)
  (:documentation "Seek to a given position."))

(defgeneric tell (string-pointer)
  (:documentation "Return the current position."))

(defmethod seek ((sp string-pointer) (position integer))
  (unless (typep position `(integer 0 ,(length (str sp))))
    (error "Position not valid for string."))
  (setf (pos sp) position)
  (values))

(defmethod tell ((sp string-pointer))
  (pos sp))

(defun search-forward-re (string-pointer regex)
  "See documentation for search-forward (with REGEX? true)"
  (multiple-value-bind (start end match-starts match-ends)
      (cl-ppcre:scan regex (str string-pointer) :start (pos string-pointer))
    (declare (ignore start))
    (when end
      (setf (pos string-pointer) end)
      (map 'vector (lambda (a b) (subseq (str string-pointer) a b))
           match-starts match-ends))))

(defun search-forward-literal (string-pointer search-for)
  "See documentation for search-forward (with REGEX? false)"
  (let ((pos (search search-for (str string-pointer)
                     :start2 (pos string-pointer))))
    (when pos
      (setf (pos string-pointer) (+ pos (length search-for)))
      t)))

(defun search-forward (string-pointer search-for &optional (regex? t))
  "Search for SEARCH-FOR in the string of STRING-POINTER, starting at the
current position. If REGEX? is true, SEARCH-FOR denotes a regular expression,
otherwise it should be a literal string. If there is a match, the current
position of STRING-POINTER is advanced to the end of the match and a vector is
returned with the substrings for the captured bits (in the case of REGEX? true)
or T in the literal case. Otherwise, NIL is returned and STRING-POINTER is
unchanged."
  (if regex?
      (search-forward-re string-pointer search-for)
      (search-forward-literal string-pointer search-for)))
