(in-package :atonews)

(defvar *proxy* '("localhost" 3128)
  "A proxy to pipe requests through. Set to nil if there isn't one.")

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

(defun ymd-to-ut (year month date)
  "Convert a YEAR, MONTH, DATE to universal time."
  (encode-universal-time 0 0 0 date month year))

(defmacro bind-scan-matches (match-starts match-ends
                             (&rest variables) &body body)
  "Call BODY with VARIABLES bound to successive bits of the match."
  (let ((st-sym (gensym)) (ed-sym (gensym)))
    `(let* ((,st-sym ,match-starts) (,ed-sym ,match-ends)
            ,@(let ((acc))
                   (do ((vars variables (cdr vars)) (n 0 (1+ n)))
                       ((null vars))
                     (when (car vars)
                       (push `(,(car vars)
                                (subseq html (elt ,st-sym ,n) (elt ,ed-sym ,n)))
                             acc)))
                   (nreverse acc)))
       ,@body)))

(defvar *debug-http-get-requests* nil
  "Set this to T to see all get requests reported.")

(defun http-get (url &key force-binary)
  "A really trivial HTTP request for a given URL. Throws an error if anything
goes wrong, and otherwise returns the contents."
  (when *debug-http-get-requests*
    (format t "~AHTTP GET ~A~%" (if force-binary "BIN " "") url)
    (finish-output))
  (multiple-value-bind (contents status headers)
      (drakma:http-request url :proxy *proxy* :force-binary force-binary)
    (unless (= 200 status) (error "Couldn't retrieve URL (~A) via Drakma" url))
    (values contents headers)))
