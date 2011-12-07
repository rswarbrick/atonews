(in-package :atonews)

(defclass mime-part () ((content-id)))

(defun mime-part? (x)
  "Return T if X is an instance of a subclass of mime-part."
  (typep x 'mime-part))

(defgeneric print-part-headers (part stream)
  (:documentation "Print the part headers for the part."))

(defmethod print-part-headers ((part mime-part) stream)
  (when (slot-boundp part 'content-id)
    (format stream "Content-ID: <~A>" (slot-value part 'content-id))
    (crlf stream))
  (values))

(defclass text-based-part (mime-part)
  ((contents :initarg :contents :reader contents)
   (mime-type :initarg :mime-type :reader mime-type)))

(defun make-text-based-part (mime-type contents)
  "Make a MIME part to hold text/html or the like."
  (make-instance 'text-based-part
                 :contents contents :mime-type mime-type))

(defun make-text-part (contents)
  "Make a MIME part to hold text."
  (make-instance 'text-based-part :contents contents :mime-type "text/plain"))

(defmethod print-part-headers :before ((part text-based-part) stream)
  (format stream "Content-type: ~A; charset=utf-8" (mime-type part))
  (crlf stream)
  (format stream "Content-transfer-encoding: quoted-printable")
  (crlf stream))

(defmethod render-to-message :before ((part mime-part) stream)
  (print-part-headers part stream)
  (crlf stream))

(defmethod render-to-message ((p text-based-part) stream)
  (quoted-printable-print (contents p) stream)
  (crlf stream))

(defclass binary-part (mime-part)
  ((mime-type :initarg :mime-type :reader mime-type)
   (octets :initarg :octets :reader octets)))

(defun make-binary-part (octets &optional (type "application/octetstream"))
  (make-instance 'binary-part
                 :mime-type type
                 :octets (coerce octets '(vector (unsigned-byte 8)))))

(defmethod print-part-headers :before ((part binary-part) stream)
  (format stream "Content-type: ~A" (mime-type part))
  (crlf stream)
  (format stream "Content-transfer-encoding: BASE64")
  (crlf stream))

(defmethod render-to-message ((p binary-part) stream)
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

(defclass mime-multipart (mime-part)
  ((parts :initarg :parts :reader parts)
   (boundary :initform (random-safe-string) :reader boundary)))

(defclass multipart-mixed (mime-multipart) ())

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

(defun random-safe-string (&optional (length 20))
  "Make a random (very) 7-bit string of the given length."
  (let* ((ranges '((65 . 90) (97 . 122) (48 . 57)))
         (max (ranges-length ranges)))
    (map-into (make-array length :element-type 'character :initial-element #\a)
              (lambda ()
                (code-char (select-from-ranges (random max) ranges))))))

(defun render-parts-between-boundaries (parts boundary stream)
  "Render the given parts to stream, separated by boundary lines (and finishing
with the closing mime boundary)"
  (dolist (subpart parts)
    (format stream "--~A" boundary)
    (crlf stream)
    (render-to-message subpart stream))
  (format stream "--~A--" boundary)
  (values))

(defmethod print-part-headers :before ((part multipart-mixed) stream)
  (format stream "Content-Type: multipart/mixed; boundary=\"~A\""
          (boundary part))
  (crlf stream))

(defmethod render-to-message ((p mime-multipart) stream)
  (render-parts-between-boundaries (parts p) (boundary p) stream))

(defgeneric content-id (part domain)
  (:documentation "Return the content-id header for the given part. If PART
didn't yet have a content-id, generate one. DOMAIN is used in the latter case
and is used as the source domain for the message (part). The content ID is in
the form XXXX@XXX.XXX (and doesn't include <> characters)"))

(defvar *content-id-counter* 0
  "An incrementing counter, used to generate globally unique Content-IDs.")

(defun generate-content-id (domain)
  "Return a Content-ID which is hopefully globally unique (not including <>)."
  (format nil "~A.~D.~D@~A"
          (random-safe-string 5)
          (incf *content-id-counter*)
          (get-universal-time)
          (or domain "dev.null.net")))

(defmethod content-id ((p mime-part) domain)
  (if (slot-boundp p 'content-id)
      (slot-value p 'content-id)
      (setf (slot-value p 'content-id)
            (generate-content-id domain))))

(defclass multipart-related (mime-multipart) ())

(defun make-multipart-related (main-part parts)
  "Make a multipart/related object. The first object to display should be
MAIN-PART, then the rest are the others."
  (unless (and (mime-part? main-part) (every #'mime-part? parts))
    (error "PARTS must all be MIME-PART objects."))
  (make-instance 'multipart-related :parts (cons main-part parts)))

(defmethod print-part-headers :before ((part multipart-related) stream)
  (format stream "Content-Type: Multipart/Related; boundary=\"~A\";"
          (boundary part))
  (crlf stream)
  (format stream "        type=\"~A\";" (mime-type (first (parts part))))
  (crlf stream)
  (format stream "        start=\"~A\"" (content-id (first (parts part)) nil))
  (crlf stream))
