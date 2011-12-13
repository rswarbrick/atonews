(in-package :atonews)

(defun tempfile-template (&optional (prefix "atonews"))
  "Return a string that can be used as a template for sb-posix:mkstemp."
  (concatenate 'string "/tmp/" prefix "XXXXXX"))

(defmacro with-named-tempfile ((fd name
                                   &optional (template (tempfile-template)))
                               &body body)
  "Create a temporary file but don't unlink it immediately. Execute BODY with FD
and NAME set to a file descriptor for and the name (as a pathname) of the
temporary file. The file is closed and unlinked when BODY finishes (with an
unwind protect)."
  (let ((name-string (gensym)))
    `(multiple-value-bind (,fd ,name-string)
         (sb-posix:mkstemp ,template)
       (let ((,name (pathname ,name-string)))
         (unwind-protect (progn ,@body)
           (sb-posix:close ,fd)
           (sb-posix:unlink ,name))))))

(defmacro with-named-tempstream ((stream name &key
                                         (element-type 'base-char)
                                         (template (tempfile-template)))
                                 &body body)
  "Create and open (:io) a temporary file as a stream with the given
ELEMENT-TYPE then execute BODY with STREAM bound to a the open stream and NAME a
pathspec pointing at the opened file."
  (let ((fd (gensym)))
    `(with-named-tempfile (,fd ,name ,template)
       (let ((,stream (sb-sys:make-fd-stream ,fd :input t :output t
                                             :element-type ,element-type
                                             :pathname ,name)))
         ;; I don't have to close the stream, because the sb-posix:close does
         ;; that for me, I think.
         ,@body))))

(defun pdf-memory-to-text (data)
  (with-named-tempstream (stream name :element-type '(unsigned-byte 8))
    (write-sequence data stream)
    (finish-output stream)
    (let ((process
           (sb-ext:run-program "/usr/bin/pdftotext" (list (namestring name) "-")
                               :output :stream
                               :wait nil))
          (acc))
      (loop
         (aif+ (read-line (sb-ext:process-output process) nil nil)
             (push it acc)
           (return
             (reduce (lambda (a b) (concatenate 'string a (string #\Newline) b))
                     (nreverse acc))))))))
