(in-package :atonews)

(defclass message ()
  ((headers :initform nil :accessor headers)
   (body :initform "" :accessor body)))

(defclass header ()
  ((key :initarg :key :reader key)
   (value :initarg :value :reader value)))

(defun make-header (key value)
  (unless (every #'quoted-printable-self-representable? key)
    (error "Invalid key for header: ~A" key))
  (make-instance 'header :key key :value value))

(defgeneric render-to-message (object stream))

(defmethod render-to-message ((h header) stream)
  (princ (rfc2047-format-header (key h) (value h)) stream)
  (fresh-line stream))

(defun make-message (from subject groups &optional body)
  (let ((msg (make-instance 'message)))
    (push (make-header "From" from) (headers msg)) 
    (push (make-header "Newsgroups" groups) (headers msg))
    (push (make-header "Subject" subject) (headers msg))
    (when body (setf (body msg) body))
    msg))

(defmethod render-to-message ((m message) stream)
  (map nil (lambda (h) (render-to-message h stream)) (headers m))
  (format stream "~%~A~%" (body m)))
