(in-package :atonews)

(defclass rss-source (http-source)
  ()
  (:documentation
   "A news source that's based on an RSS feed. To use this, you probably don't
need to alter the message fragment machinery at all, but you may wish to
override FILTER-SOURCE-CONTENTS."))

(defclass rss-message-fragment (message-fragment)
  ((description :reader description)
   (url :reader url))
  (:documentation
   "RSS sources have descriptions already: we probably don't follow the URL in
the link, so cache this here in order to be able to use it when making the full
message. This doesn't inherit from HTTP-MESSAGE-FRAGMENT, because the meaning of
URL is rather different."))

(defun rss-uri-to-message-id (uri referrer)
  (format nil "<~A@~A>"
          (cl-base64:string-to-base64-string (sb-md5:md5sum-string uri))
          (puri:uri-host (puri:parse-uri referrer))))

(defun rss-make-message-fragment (item channel from-address)
  (let ((frag (make-message-fragment
               (rss-uri-to-message-id (rss:link item) (rss:link channel))
               (rss:title item)
               from-address
               :date (or (rss:pub-date item) (rss:pub-date channel))
               :url (rss:link item)
               :class 'rss-message-fragment)))
    (setf (slot-value frag 'description) (rss:description item))
    frag))

(defmethod find-message-fragments ((source rss-source) contents)
  (let* ((channel (rss:parse-rss-stream contents :err t :strict? t))
         (from-address (format nil "dev.null@~A"
                               (puri:uri-host (puri:parse-uri
                                               (rss:link channel))))))
    (mapcar (lambda (item)
              (rss-make-message-fragment item channel from-address))
            (rss:items channel))))

(defmethod expand-message-fragment ((source rss-source)
                                    (fragment rss-message-fragment))
  (values "text/html"
          (format nil "~
<html><head><title>~A</title></head>~
<body>~A<br><br>~
<p><a href=\"~A\">Link</a></p></body>"
                  (subject fragment)
                  (description fragment)
                  (url fragment))))
