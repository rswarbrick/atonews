(in-package :atonews)

(defclass nntp-server ()
  ((host :initarg :host :reader host)
   (port :initarg :port :type '(integer 0) :reader port)))

(defun make-server (&optional (host "localhost") (port 119))
  (make-instance 'nntp-server :host host :port port))

(defclass news-connection ()
  ((server :initarg :server :reader server)
   (socket :initarg :socket :accessor socket)
   (dbg :initarg :debug :reader dbg)))

(defmethod make-connection ((server nntp-server) &optional debug)
  (handler-case
      (make-instance 'news-connection
                     :server server :debug debug
                     :socket (usocket:socket-connect (host server) (port server)))
    (usocket:ns-error (c) (declare (ignore c)) nil)))

(defmethod close ((c news-connection) &key abort)
  "Close the NEWS-CONNECTION. Ignores ABORT (only there to avoid conflicting
with gray-stream's close)"
  (declare (ignore abort))
  (when (socket c)
    (usocket:socket-close (socket c))
    (setf (socket c) nil)))

(defmethod say ((str string) (c news-connection))
  (unless (socket c) (error "Connection closed"))
  (let ((stream (usocket:socket-stream (socket c))))
    (when (dbg c)
      (format t "WRITE:~%~A~%END WRITE.~%" str)
      (force-output))
    (princ str stream) (fresh-line stream) (force-output stream))
  (values))

(defun strip-trailing-cr (str)
  "Returns STR but without any (single) trailing ^M."
  (if (eql (elt str (1- (length str))) #\Return)
      (subseq str 0 (1- (length str)))
      str))

(defmethod hear ((c news-connection) type)
  "Get a response from the news server. TYPE should be :TEXT or :STATUS for the
  type of response we're expecting. Strips the trailing ^M"
  (when (dbg c)
    (format t "READ. Type ~A~%" type)
    (force-output))
  (handler-case
      (cond
        ;; Closed connection
        ((not (socket c)) nil)
        ;; Single status line
        ((eq type :STATUS)
         (let* ((line (read-line (usocket:socket-stream (socket c)))))
           (when (dbg c)
             (format t "-> ~A~%" line)
             (force-output))
           (values (parse-integer (subseq line 0 3))
                   (strip-trailing-cr
                    (subseq line (if (eql (elt line 3) #\ ) 4 3))))))
        ;; Lines terminated with . CR/LF
        ((eq type :TEXT)
         (do ((lines nil))
             (())
           (let ((line (read-line (usocket:socket-stream (socket c)))))
             (when (dbg c)
               (format t "-> ~A~%" line)
               (force-output))
             (cond
               ((eql (elt line 0) #\.)
                (if (= (length line) 2)
                    (return (nreverse lines))
                    (push (strip-trailing-cr (subseq line 1)) lines)))
               (t
                (push (strip-trailing-cr line) lines))))))
        (t
         (error "Unrecognised TYPE. Use :TEXT or :STATUS.")))
    (end-of-file (cnd)
      (declare (ignore cnd))
      (close c))))

(defun expect-status (expected connection &optional trying-to)
  "Check we get the expected status, otherwise throw an error."
  (multiple-value-bind (stat reply) (hear connection :status)
    (unless (= stat expected)
      (error "Server unhappy~@[ when trying to ~A~]. Error: ~D. (~A)"
             trying-to stat reply))))

(defmethod quit ((c news-connection))
  "Say goodbye and close the connection. Don't bother returning anything: the
only answer is 205."
  (say "quit" c)
  (hear c :status)
  (close c)
  (values))

(defmethod post ((c news-connection) (m message))
  "Try to post the given message to the connection."
  (say "post" c)
  (expect-status 340 c "initiate posting")
  (say
   (with-output-to-string (s)
     (render-to-message m s)
     (fresh-line s)
     (princ "." s)
     (terpri s))
   c)
  (expect-status 240 c "post message"))

(defmacro with-connection ((conn-sym server &optional dbg) &body body)
  "Run the forms in BODY with CONN-SYM bound to a connection described by
SERVER."
  `(let ((,conn-sym (make-connection ,server ,dbg)))
     (expect-status 200 ,conn-sym "establish connection")
     (unwind-protect (progn ,@body)
       (quit ,conn-sym))))
