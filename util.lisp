(in-package :atonews)

(defun negate (predicate)
  "PREDICATE should be a function of one argument. Returns a function that is
the logical negation of PREDICATE."
  (lambda (x) (not (funcall predicate x))))

(defun predicate-blocks (predicate sequence)
  "Return a list of the blocks of SEQUENCE correspond to a run of elements
matching/failing to match PREDICATE. Also returns whether the first block
matches the predicate or not."
  (let (last-match first-match (n 0) (start 0) (lst))
    (map nil
         (lambda (x)
           (let ((px (funcall predicate x)))
             (cond
               ((= n 0)
                (setf first-match px
                      last-match px))
               ((or (and px (not last-match))
                    (and (not px) last-match))
                (push (subseq sequence start n) lst)
                (setf last-match px
                      start n)))
             (incf n)))
         sequence)
    (push (subseq sequence start) lst)
    (values (nreverse lst) first-match)))

(defun decompose-integer (n base)
  "Decompose N into a list (most significant part first) of numbers each less
than BASE so that the sum of these numbers with increasing powers of BASE is N."
  (let ((acc))
    (do* ((k n (floor k base)))
         ((= k 0) acc)
      (push (mod k base) acc))))

(defun crlf (stream)
  "Output #\Return #\Newline to stream."
  (princ #\Return stream)
  (princ #\Newline stream)
  (values))

(defmacro aif+ (test then &body else)
  "Anaphoric IF, which also allows multiple forms in the ELSE part."
  `(let ((it ,test)) (if it ,then ,@else)))

(defmacro awhen (test &body then)
  "Anaphoric WHEN."
  `(let ((it ,test)) (when it ,@then)))

(defun map-find (func sequence &key from-end (start 0) end)
  "Apply FUNC to each element of SEQUENCE in turn until it returns a true
result. Return this true result."
  (let ((ans nil))
    (position-if (lambda (x) (setf ans (funcall func x)))
                 sequence :from-end from-end :start start :end end)
    ans))

(defun hash-values (hashtable)
  "Return all the values in HASHTABLE."
  (let ((acc nil))
    (maphash (lambda (key val) (declare (ignore key)) (push val acc)) hashtable)
    acc))
