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

(defun rss-uri-to-message-id (uri host)
  (format nil "<~A@~A>"
          (cl-base64:string-to-base64-string (sb-md5:md5sum-string uri))
          host))

(defun rss-make-message-fragment (item channel from-address host)
  (let ((frag (make-message-fragment
               (rss-uri-to-message-id (rss:link item) host)
               (rss:title item)
               from-address
               :date (or (rss:pub-date item) (rss:pub-date channel))
               :url (rss:link item)
               :class 'rss-message-fragment)))
    (setf (slot-value frag 'description) (rss:description item))
    frag))

(defmethod get-header-data :around ((ns rss-source))
  (let ((drakma:*text-content-types*
         (cons '("application" . "rss+xml") drakma:*text-content-types*)))
    (call-next-method)))

(defmethod find-message-fragments ((source rss-source) contents)
  (let* ((channel (rss:parse-rss-stream contents :err t :strict? nil))
         (host (puri:uri-host
                (puri:parse-uri (if (< 0 (length (rss:link channel)))
                                    (rss:link channel) (list-url source)))))
         (from-address (format nil "dev.null@~A" host)))
    (mapcar (lambda (item)
              (rss-make-message-fragment item channel from-address host))
            (rss:items channel))))

(defmethod contents-for-message-fragment ((source rss-source)
                                         (fragment rss-message-fragment))
  (format nil "~
<html><head><title>~A</title></head>~
<body>~A<br><br>~
<p><a href=\"~A\">Link</a></p></body>"
          (subject fragment)
          (description fragment)
          (url fragment)))

(defclass rss-link-source (rss-source)
  ()
  (:documentation
   "Like RSS-SOURCE, but FILTER-SOURCE-CONTENTS is given the contents of the
linked URL, rather than the feed post. Useful for RSS feeds that basically just
contain a list of titles."))

(defmethod contents-for-message-fragment ((source rss-link-source)
                                          (fragment rss-message-fragment))
  (http-get (url fragment)))
