(in-package :atonews)

(defclass mime-part ()
  ())

(defun mime-part? (x)
  "Return T if X is an instance of a subclass of mime-part."
  (typep x 'mime-part))

(defclass text-part (mime-part)
  ((contents :initarg :contents :reader contents)))

(defun make-text-part (contents)
  "Make a MIME part to hold text."
  (make-instance 'text-part :contents contents))

(defmethod render-to-message ((p text-part) stream)
  (format stream "Content-type: text/plain; charset=utf-8")
  (crlf stream)
  (format stream "Content-transfer-encoding: quoted-printable")
  (crlf stream)
  (crlf stream)
  (quoted-printable-print (contents p) stream)
  (crlf stream))

(defclass binary-part (mime-part)
  ((mime-type :initarg :mime-type :reader mime-type)
   (octets :initarg :octets :reader octets)))

(defun make-binary-part (octets &optional (type "application/octetstream"))
  (make-instance 'binary-part
                 :mime-type type
                 :octets (coerce octets '(vector (unsigned-byte 8)))))

(defmethod render-to-message ((p binary-part) stream)
  (format stream "Content-type: ~A" (mime-type p))
  (crlf stream)
  (format stream "Content-transfer-encoding: BASE64")
  (crlf stream) (crlf stream)
  (let ((enc (cl-base64:usb8-array-to-base64-string (octets p))))
    (do ((pos 0 (+ pos 76)))
        ((>= pos (length enc)))
      (princ (subseq enc pos (min (+ pos 76) (length enc))) stream)
      (crlf stream))))

(defun slurp-binary-stream (stream)
  (let ((dest (make-array (file-length stream)
                          :element-type '(unsigned-byte 8)
                          :initial-element 0)))
    (read-sequence dest stream)
    dest))

(defun make-binary-part-from-file (filespec
                                   &optional (type "application/octetstream"))
  "Utility function to quickly test binary part manufacture."
  (with-open-file (s filespec :element-type '(unsigned-byte 8))
    (make-binary-part (slurp-binary-stream s) type)))

(defclass multipart-mixed (mime-part)
  ((parts :initarg :parts :reader parts)))

(defun make-multipart-mixed (&rest parts)
  (unless (every #'mime-part? parts)
    (error "PARTS must all be MIME-PART objects."))
  (make-instance 'multipart-mixed :parts parts))

(defun select-from-ranges (i ranges &optional wraparound?)
  "RANGES should be a list of pairs of integers [a, b]. Then we return the I'th
element from the intervals laid end to end. If WRAPAROUND? is true, then when I
is too large, we go back to the start."
  (do ((ranges-left ranges (cdr ranges-left)))
      ((<= i (- (cdar ranges-left) (caar ranges-left)))
       (+ i (caar ranges-left)))
    (decf i (1+ (- (cdar ranges-left) (caar ranges-left))))
    (unless (cdr ranges-left)
      (if wraparound?
          (setf ranges-left (cons nil ranges))
          (error "I too large.")))))

(defun ranges-length (ranges)
  "RANGES should be pairs of integers as for select-from-ranges. Return the
total length of the ranges."
  (reduce #'+ ranges :key (lambda (range) (1+ (- (cdr range) (car range))))))

(defun multipart-boundary (&optional (length 20))
  "Make a random multipart boundary."
  (let* ((ranges '((65 . 90) (97 . 122) (48 . 57)))
         (max (ranges-length ranges)))
    (map-into (make-array length :element-type 'character :initial-element #\a)
              (lambda ()
                (code-char (select-from-ranges (random max) ranges))))))

(defmethod render-to-message ((p multipart-mixed) stream)
  (let ((bdry (multipart-boundary)))
    (format stream "Content-Type: multipart/mixed; boundary=\"~A\"" bdry)
    (crlf stream) (crlf stream)
    (dolist (subpart (parts p))
      (format stream "--~A" bdry)
      (crlf stream)
      (render-to-message subpart stream))
    (format stream "--~A--" bdry)))
