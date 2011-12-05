(in-package :atonews)

;; (Literal representation) Octets with decimal values of
;; 33 through 60 inclusive, and 62 through 126, inclusive,
;; MAY be represented as the US-ASCII characters which
;; correspond to those octets (EXCLAMATION POINT through
;; LESS THAN, and GREATER THAN through TILDE,
;; respectively).
(defun quoted-printable-self-representable? (c)
  (or (<= 33 (char-code c) 60)
      (<= 62 (char-code c) 126)))

(defun ascii-horizontal-whitespace? (c)
  "Return T if C is a space or a tab."
  (member c '(#\Space #\Tab)))

(defun consume-notrans (chars-left str)
  "Returns APPENDME, FULL?, UNUSED"
  (let ((length (length str)))
    (cond
      ((>= chars-left length)
       (values str (= chars-left length) nil))
      (t
       (values (subseq str 0 chars-left) t (subseq str chars-left))))))

(defun encode-char (ch)
  "Returns a string that encodes the given char"
  (if (quoted-printable-self-representable? ch)
      (string ch)
      (format nil "~{=~2,'0X~}"
              (coerce (babel:string-to-octets (string ch)) 'list))))

(defun consume-trans (chars-left str &optional last?)
  "Returns APPENDME, FULL?, UNUSED"
  (declare (ignore last?))
  (let ((encode-me? (not (every #'quoted-printable-self-representable? str))))
    (if (not encode-me?)
        (consume-notrans chars-left str)
        (let ((acc) (first-unused))
          (decf chars-left 12)
          (dotimes (n (length str))
            (let ((rep (encode-char (aref str n))))
              (when (> (length rep) chars-left)
                (setf first-unused n)
                (return))
              (decf chars-left (length rep))
              (push rep acc)))
          (if (and first-unused (= 0 first-unused))
              (values nil t str)
              (values (format nil "=?utf-8?Q?~{~A~}?=" (nreverse acc))
                      (or (and first-unused t) (= 0 chars-left))
                      (and first-unused (subseq str first-unused))))))))

(defun consume-space (chars-left str last?)
  "Returns APPENDME, FULL?, UNUSED. Irritatingly, I mustn't leave trailing
whitespace on a line, so if I realise I would do that, I need to encode the
stuff. To ensure that I don't end the line, I need LAST? to be false."
  (if (not last?)
      (consume-notrans chars-left str)
      (consume-trans chars-left str)))

(defun encode-2047-header (str chars-left)
  "Return a string encoding STR such that each line is at most 76 characters
long, including the =?...?= bits and such that the first piece is at most
CHARS-LEFT long."
  (multiple-value-bind (blocks space?)
      (predicate-blocks #'ascii-horizontal-whitespace? str)
    (let ((lines) (acc ""))
      (flet ((take (str)
               (setf acc (concatenate 'string acc str))
               (decf chars-left (length str)))
             (store ()
               (push acc lines)
               (setf acc " ")
               (setf chars-left 75)))
        (do ((blocks-left blocks (cdr blocks-left)))
            ((null blocks-left))
          (let ((last? (null (cdr blocks-left)))
                (this-block (car blocks-left)))
            (loop 
               (multiple-value-bind (appendme full? unused)
                   (funcall (if space? #'consume-space #'consume-trans)
                            (1- chars-left) this-block last?)
                 (when appendme (take appendme))
                 (when full? (store))
                 (if unused
                     (setf this-block unused)
                     (return)))))
          (setf space? (not space?)))
        (format nil "~{~A=~%~}~A" (nreverse lines) acc)))))
