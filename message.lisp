(in-package :atonews)

(defclass message ()
  ((headers :initform nil :accessor headers)
   (body :initform "" :accessor body)))

(defclass header ()
  ((key :initarg :key :reader key)
   (value :initarg :value :reader value)))

(defmethod print-object ((header header) stream)
  (print-unreadable-object (header stream :type t)
    (format stream "~A = ~A" (key header) (value header))))

(defun make-header (key value)
  (unless (every #'quoted-printable-self-representable? key)
    (error "Invalid key for header: ~A" key))
  (make-instance 'header :key key :value value))

(defmethod render-to-message ((h header) stream)
  (princ (rfc2047-format-header (key h) (value h)) stream)
  (crlf stream))

(defun make-message (from subject groups &optional body &rest other-headers)
  "Make a message with the given From:, Subject: and Newsgroups: headers. If
given, also set the body. OTHER-HEADERS should be a list (of even length) of
pairs of header names and values."
  (unless (evenp (length other-headers))
    (error "OTHER-HEADERS should have even length."))
  (let ((msg (make-instance 'message)))
    (push (make-header "From" from) (headers msg)) 
    (push (make-header "Newsgroups" groups) (headers msg))
    (push (make-header "Subject" subject) (headers msg))
    (do ((rest other-headers (cddr rest)))
        ((null rest))
      (let ((header-name (first rest))
            (header-value (second rest)))
        (push (make-header header-name header-value) (headers msg))))
    (when body (setf (body msg) body))
    msg))

(defmethod render-to-message ((str string) stream)
  ;; Copy the string straight through. This shouldn't usually be used since it
  ;; doesn't deal with CRLF problems or with 8bit characters.
  (princ str stream)
  (crlf))

(defmethod render-to-message ((m message) stream)
  (map nil
       (lambda (h) (render-to-message h stream))
       (headers m))
  ;; MIME parts do the double crlf themselves, since they have a couple of their
  ;; own headers to put in before the end of the header section.
  (if (mime-part? (body m))
      (render-to-message (make-header "MIME-Version" "1.0") stream)
      (crlf stream))
  (render-to-message (body m) stream)
  (values))
