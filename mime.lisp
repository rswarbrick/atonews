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
