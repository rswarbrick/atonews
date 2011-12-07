(in-package :atonews)

(defvar *rfc2047-header-encoding-alist*
  '(("^Newsgroups" . nil)
    ("^Followup-To" . nil)
    ("^Message-ID" . nil)
    ("(Resent-)?(From|Cc|To|Bcc|(In-)?Reply-To|Sender|\
Mail-Followup-To|Mail-Copies-To|Approved)"
     . address-mime)
    (t . mime))
  "From Gnus. Use nil for no encoding needed; MIME for ordinary escaping and
ADDRESS-MIME for addresses.")

(defun encoding-method (header-name)
  "Return the method to use (nil, mime or address-mime) to encode the given
header."
  (cdr (find-if
        (lambda (first) (or (eq first t) (cl-ppcre:scan first header-name)))
        *rfc2047-header-encoding-alist*
        :key #'car)))

(defun skip-chars (seq index skip-over &key negate? backwards)
  "Skips forwards over elements of SEQ in SKIP-OVER, starting at INDEX. If
NEGATE?, then skips elements not in SKIP-OVER. If BACKWARDS is true, instead run
backwards starting at index and end at the last character (backwards) in
SKIP-OVER that we see."
  (let* ((func (if negate? #'position-if #'position-if-not))
         (keyargs (list :from-end backwards
                        (if backwards :end :start) index))
         (pos (apply func (lambda (x) (find x skip-over)) seq
                     keyargs)))
    (if backwards (1+ (or pos -1)) (or pos (length seq)))))

(defun skip-forward-whitespace (str index &optional negate)
  "Skips forwards over whitespace in STR, starting at INDEX. If NEGATE, then
skips non-whitespace."
  (skip-chars
   str index (coerce #(#\Space #\Linefeed #\Tab) 'string) :negate? negate))

(defun skip-forward-word (str index)
  "Skip forwards over whitespace, then non-whitespace in STR starting at
INDEX (ie one word)."
  (let ((end-space (skip-forward-whitespace str index)))
    (skip-forward-whitespace str end-space t)))

(defun rfc2047-needs-encoding? (str)
  "Return T if we need to do something non-trivial to encode the given string."
  (or (not (every (lambda (c) (<= (char-code c) 126)) str))
      (and (search "=?" str) (search "?=" str))))

(defun quoted-printable-self-representable? (c)
  (or (<= 33 (char-code c) 60)
      (<= 62 (char-code c) 126)))

(defun skip-forward-words-needing-encoding (str index)
  "Skips forwards over elements of STR that need encoding, starting at
index. Returns the position at the end of the last successive word that needs
encoding. This will either be at whitespace or at the end of STR."
  (let ((pos index) (end index))
    (loop
       (when (= pos (length str)) (return))
       (setf end (skip-forward-word str pos))
       (unless (rfc2047-needs-encoding? (subseq str pos end)) (return))
       (setf pos end))
    pos))

(defun qp-encode-char (ch)
  "Definitely encode the given character in quoted-printable encoding."
  (format nil "~{=~2,'0X~}"
          (coerce (babel:string-to-octets (string ch)) 'list)))

(defun rfc2047-encode-char (ch)
  "Returns a string that encodes the given char"
  (if (quoted-printable-self-representable? ch) (string ch) (qp-encode-char ch)))

(defun rfc2047-encode (str start end &optional (max-quot-length 75))
  "Encode the subsequence as quoted printable as per rfc2047."
  (let ((quots) (acc) (length 0))
    (flet ((store ()
             (push (format nil "=?utf-8?Q?~{~A~}?=" (nreverse acc)) quots)
             (setf length 0)))
      (do ((n start (1+ n)))
          ((= n end))
        (let ((encoded (rfc2047-encode-char (aref str n))))
          (when (> (+ 12 (length encoded) length) max-quot-length)
            (store))
          (push encoded acc)
          (incf length (length encoded))))
      (store)
      (reduce (lambda (a b) (concatenate 'string a " " b)) (nreverse quots)))))

(defun rfc2047-encode-header (key value)
  "Encode the given header correctly for RFC 2047. Returns the string for the
encoded value."
  ;; Based on the source of rfc2047.el from Gnus.
  (let ((method (encoding-method key))
        (start 0) (pos 0))
    (cond
      ((not (and method (rfc2047-needs-encoding? value)))
       ;; Either no method used or no encoding required.
       value)
      ((eq method 'mime)
       ;; This is the "easy case".
       (loop
          ;; Skip forward over any whitespace
          (setf start (skip-forward-whitespace value start))
          (when (= start (length value)) (return))
          ;; Walk over words that need encoding and clump them together to do in
          ;; one batch.
          (setf pos (skip-forward-words-needing-encoding value start))
          (if (> pos start)
              (let ((encoded-section (rfc2047-encode value start pos)))
                (setf value
                      (concatenate 'string
                                   (subseq value 0 start)
                                   encoded-section
                                   (subseq value pos))
                      start (+ start (length encoded-section))))
              ;; The next word is presumably fine, so skip over it.
              (setf start (skip-forward-word value pos))))
       value)
      ((eq method 'address-mime)
       ;; This is massively more complicated, in theory. So I'm going to punt on
       ;; the difficulty. I'll throw an error if I *should* have done
       ;; something. Then if I start getting errors, I'll write the code :-) Of
       ;; course, I checked earlier for not needing encoding, so I'll just throw
       ;; an error here.
       (error "ADDRESS-MIME encoding not yet ported."))
      (t
       (error "Unknown encoding method: ~A" method)))))

(defun looking-at? (string regex &key (start 0))
  "Return TRUE iff there is a match for REGEX in STRING, starting at position
START."
  (and (cl-ppcre:scan (append '(:sequence :start-anchor)
                              (list (cl-ppcre:parse-string regex)))
                      string :start start)
       t))

(defun rfc2047-fold (str &optional (beginning-of-line 0))
  "Fold STR as per rfc2047. Make BEGINNING-OF-LINE negative if there was a
header earlier. Acts destructively on STR."
  ;; Ported semi-intelligently from Gnus
  (let ((out)
        (first t)
        (pos 0)
        (bol beginning-of-line)
        (word-start nil)
        (qword-start nil))
    (labels ((at-end? () (= pos (length str)))
             (at-this? (x) (eq (aref str pos) x))
             (skip-over (chars &optional negate?)
               (setf pos (skip-chars str pos chars :negate? negate?)))
             (skip-nonwhite ()
               (skip-over
                (coerce '(#\Space #\Tab #\Newline #\Return) 'string) t))
             (process-break ()
               (when (and (or word-start qword-start)
                          (> (- pos bol) 76))
                 (setf pos (skip-chars str (or word-start qword-start)
                                       '(#\Space #\Tab) :backwards t)
                       word-start nil
                       qword-start nil)
                 (unless (> pos bol)
                   (error "Could not find a suitable break point."))
                 (push (subseq str (max 0 bol) pos) out)
                 ;; This is the bit where we act destructively on STR. If the
                 ;; next character isn't a space, we need one, so replace the
                 ;; previous character by one.
                 (unless (looking-at? str "[ \t]" :start pos)
                   (setf (aref str (1- pos)) #\Space)
                   (decf pos))
                 (setf bol pos)
                 ;; We don't want to break again before the first whitespace, so
                 ;; jump forward.
                 (skip-over '(#\Space #\Tab))
                 (unless (at-end?) (incf pos)))))
      (loop
         (when (at-end?) (return))
         ;; If we needed to break the string, now is the time to do it.
         (process-break)
         (cond
           ((at-this? #\Newline)
            (incf pos)
            (setf bol pos
                  word-start nil
                  qword-start nil
                  pos (skip-forward-whitespace str pos))
            ;; Eat at least one non-space char
            (unless (or (at-end?) (at-this? #\Newline))
              (incf pos)))
           ((at-this? #\Return)
            (incf pos))
           ((member (aref str pos) '(#\Space #\Tab))
            (skip-over '(#\Space #\Tab))
            (unless first (setf word-start pos)))
           ((not word-start)
            ;; At the moment, we don't have any next place that we can break, so
            ;; skip forward until we find somewhere to do so. Of course, we'd
            ;; better be careful to skip qwords correctly.
            (if (looking-at? str "=\\?[^=]" :start pos)
                (progn
                  (unless first (setf qword-start pos))
                  (skip-over '(#\Space #\Tab #\Newline #\Return #\=) t))
                (if (at-this? #\=)
                    (incf pos)
                    (skip-nonwhite))))
           (t
            (skip-nonwhite)))
         (setf first nil))
      (process-break))
    (nreverse (cons (subseq str (max bol 0)) out))))

(defun rfc2047-format-header (name value)
  "Return a sendable version of the given header data."
  (format nil
          (concatenate 'string
                       "~A: ~{~A~^"
                       (string #\Return)
                       "~%~}")
          name 
          (rfc2047-fold (rfc2047-encode-header name value)
                        (- (+ 2 (length name))))))

(defun quoted-printable-print (text stream)
  "Write the given TEXT to STREAM in quoted-printable encoding. Assumes that
we're starting at the beginning of a line, so have 76 characters to go."
  (let ((pos 0) (line-pos 0))
    (labels
        ((send-it (seq) (princ seq stream) (incf line-pos (length seq)))
         (newline (&optional soft?)
           (when soft? (princ #\= stream)) (crlf stream) (setf line-pos 0))
         (enc-putc ()
           (let ((encoded (qp-encode-char (aref text pos))))
             (when (> (+ line-pos (length encoded)) 75)
               (newline t))
             (send-it encoded)))
         (copy-until (end) (send-it (subseq text pos end))))
      (loop
         (cond
           ((eq pos (length text))
            (return))

           ((eq (aref text pos) #\Newline)
            (newline)
            (incf pos))

           ((or (<= 33 (char-code (aref text pos)) 60)
                (<= 62 (char-code (aref text pos)) 126))
            (when (= line-pos 75) (newline t))
            (princ (aref text pos) stream)
            (incf line-pos)
            (incf pos))

           ((looking-at? text "[ \t]" :start pos)
            ;; Trailing spaces need encoding
            (when (= line-pos 75) (newline t))
            (let ((end-ws (skip-chars text pos '(#\Space #\Tab))))
              (if (or (= end-ws (length text))
                      (member (aref text end-ws) '(#\Newline #\Return))
                      (> (+ (- end-ws pos) line-pos) 75))
                  (enc-putc)
                  (progn
                    (copy-until end-ws)
                    (setf pos (1- end-ws))))
              (incf pos)))

           (t
            (enc-putc)
            (incf pos)))))))

