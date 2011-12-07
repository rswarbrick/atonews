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
