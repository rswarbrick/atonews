(in-package :atonews)

(defvar *proxy* '("localhost" 3128)
  "A proxy to pipe requests through. Set to nil if there isn't one.")

(defclass news-source ()
  ())

(defclass http-source (news-source)
  ((list-url :reader list-url
             :documentation "The URL for the listing page."))
  (:documentation
   "A news source where you get the data via http. To use this, define a method
   on PARSE-SOURCE-HEADERS and return HTTP-MESSAGE-FRAGMENTs."))

(defclass message-fragment ()
  ((id :initarg :id :reader id)
   (from :initarg :from :accessor from)
   (date :initarg :date :accessor date)
   (subject :initarg :subject :accessor subject)))

(defclass http-message-fragment (message-fragment)
  ((url :initarg :url :reader url)))

(defgeneric list-headers (news-source)
  (:documentation
   "Return a list of message fragments corresponding to the new articles
available from the news source. It doesn't matter if a previously seen article
is returned, but in that case it should have the same Message-id as before."))

(defgeneric parse-source-headers (news-source contents)
  (:documentation
   "Responsible for parsing the data retrieved by the news source and making
message fragments."))

(defgeneric expand-message-fragment (news-source fragment)
  (:documentation
   "Responsible for returning the contents of the message denoted by
fragment. Returns (VALUES MIME-TYPE DATA)"))

(defgeneric filter-source-contents (news-source data stream)
  (:documentation
   "DATA is a string buffer holding some data (probably pulled from a URL). This
function must write data to STREAM, a text stream and return the relevant
mime-type."))

(defgeneric make-message-from-fragment (news-source fragment destination)
  (:documentation
   "Responsible for producing a (probably multipart mime) message from the given
fragment. DESTINATION is the address that should go in the Newsgroups: line."))

(defgeneric author-address (message-or-source)
  (:documentation
   "Return the correct From: address to use for the message. Specialise for
either your message fragment (higher precedence) or your news source."))

(defmethod author-address ((src news-source)) nil)
(defmethod author-address ((frag message-fragment)) nil)

(defun universal-time-to-2822 (ut &optional time-zone)
  "Return a string representing the time given by UT in the format mandated by
RFC-2822. Uses the local time-zone if TIME-ZONE is not supplied."
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time ut time-zone)
    (declare (ignore daylight-p))
    (format nil "~A, ~D ~A ~D ~2,'0D:~2,'0D:~2,'0D ~C~4,'0D"
            (elt '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") day)
            date
            (elt '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                   "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") (1- month))
            year
            hour minute second
            (if (>= zone 0) #\+ #\-)
            (floor (* zone 100)))))

(defun make-message-fragment (id subject from &key date url)
  "Make a new message fragment with the given ID, SUBJECT and author (FROM). All
four arguments should be strings and, if DATE is not given, it is set to the
current date."
  (let ((mf
         (make-instance (if url 'http-message-fragment 'message-fragment)
                        :id id
                        :from from
                        :subject subject
                        :date (or date (universal-time-to-2822
                                        (get-universal-time))))))
    (when url
      (setf (slot-value mf 'url) url))
    mf))

(defmethod list-headers ((source http-source))
  (multiple-value-bind (contents status)
      (drakma:http-request (list-url source) :proxy *proxy*)
    (unless (= 200 status)
      (error "Couldn't retrieve URL (~A) via Drakma" (list-url source)))
    (parse-source-headers source contents)))

(defmethod expand-message-fragment ((source news-source)
                                    (fragment http-message-fragment))
  (multiple-value-bind (contents status)
      (drakma:http-request (url fragment) :proxy *proxy*)
    (unless (= 200 status)
      (error "Couldn't retrieve URL (~A) via Drakma" (list-url source)))
    (let* ((mime-type nil)
           (data (with-output-to-string (str)
                   (setf mime-type
                         (filter-source-contents source contents str)))))
      (values mime-type data))))

(defun url-domain (url)
  "Get the domain of a fully-qualified url."
  (aref (nth-value
         1 (cl-ppcre:scan-to-strings
            (cl-ppcre:create-scanner "^[a-zA-Z0-9]+://([^/]*)") url))
        0))

(defun resolve-url (url location)
  "Try to resolve the given URL given that we're currently at LOCATION."
  (if (cl-ppcre:scan "^[a-zA-Z0-9]+://" url)
      url
      (let ((regex (if (eql (char url 0) #\/)
                       (cl-ppcre:create-scanner
                        "^[a-zA-Z0-9]+://[^/]*")
                       (cl-ppcre:create-scanner
                        "^[a-zA-Z0-9]+://[^/]*(?:[^/]*?/)*"))))
        (aif+ (cl-ppcre:scan-to-strings regex location)
            (concatenate 'string
                         it
                         (if (eql (char it (1- (length it))) #\/) "" "/")
                         (if (eql (char url 0) #\/) (subseq url 1) url))
          (error "Location is not a fully qualified URL.")))))

(defun fixup-html-links (fragment html)
  "Go through HTML as follows: For each img src, if I can download the relevant image, encode it and replace the link with a CID one. If not, replace it with a fully qualified URL (the news viewer may or may not complain, but it's the most useful thing we can do). Also try to replace relative URLs in <a href=...> links with sensible ones. Returns (VALUES FIXED-HTML BINARY-PARTS)."
  (let ((scratch html)
        (parts (make-hash-table :test 'equal))
        (domain (url-domain (url fragment))))
    ;; Fix <img> links
    (setf scratch
          (cl-ppcre:regex-replace-all
           (cl-ppcre:create-scanner
            "<img(.*?)src=((?:\"|')(.*?)(?:\"|')|[^\"][^ >]*)" :single-line-mode t)
           scratch
           (lambda (full-match gubbins big-url trimmed-url)
             (declare (ignore full-match))
             (let ((resolved-url (resolve-url (or trimmed-url big-url)
                                              (url fragment))))
               (format nil "<img~Asrc=\"cid:~A\""
                       gubbins
                       (content-id
                        (or (gethash resolved-url parts)
                            (setf (gethash resolved-url parts)
                                  (get-mime-image resolved-url)))
                        domain))))
           :simple-calls t))
    ;; And <a> links
    (setf scratch
          (cl-ppcre:regex-replace-all
           (cl-ppcre:create-scanner
            "<a([^>]*?)href=((?:\"|')(.*?)(?:\"|')|[^\"][^ >]*)" :single-line-mode t)
           scratch
           (lambda (full-match gubbins big-url trimmed-url)
             (declare (ignore full-match))
             (format nil "<a~Ahref=\"~A\""
                     gubbins
                     (resolve-url (or trimmed-url big-url) (url fragment))))
           :simple-calls t))
    (values scratch (hash-values parts))))

(defun get-mime-image (url)
  "Download the given URL and return a mime-part representing the image."
  (multiple-value-bind (contents status headers)
      (drakma:http-request url :force-binary t :proxy *proxy*)
    (unless (= status 200)
      (error "Error downloading image."))
    (make-binary-part contents
                      (cdr (assoc :content-type headers)))))

(defmethod make-message-from-fragment ((source news-source)
                                       (fragment message-fragment)
                                       destination)
  (multiple-value-bind (mime-type body-data)
      (expand-message-fragment source fragment)
    (let ((other-parts nil))
      (when (string= mime-type "text/html")
        (multiple-value-setq (body-data other-parts)
          (fixup-html-links fragment body-data)))
      (let ((main-part (make-text-based-part mime-type body-data)))
        (make-message (or (author-address fragment)
                          (author-address source))
                      (subject fragment)
                      destination
                      (if other-parts
                          (make-multipart-related main-part other-parts)
                          main-part))))))
