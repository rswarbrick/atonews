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
