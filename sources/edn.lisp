(in-package :atonews)

(defclass edn-design-ideas-ns (rss-link-source)
  ((list-url :initform "http://www.edn.com/rss/designideas")))

(defparameter *edn-header-regexp*
  (cl-ppcre:create-scanner
   (concatenate 'string
                "<h2>.*?<a href=\"/([^\"]+)[^>]*?>([^<]+).*?pubDate\">"
                "\\s*([0-9]+)\\s*/\\s*([0-9]+)\\s*/\\s*([0-9]+)\\s*")
   :single-line-mode t)
  "Extracts URL, Title, Year, Month, Day")

(defun edn-extract-article-number (url)
  (multiple-value-bind (all matches)
      (ppcre:scan-to-strings "([0-9]+)[^/]*$" url)
    (if all (elt matches 0) "0")))

(defun edn-make-id (url y m d)
  (format nil "<~A-~A~A~A@edn.com>"
          (edn-extract-article-number url) y m d))

(defmethod filter-source-contents ((source edn-design-ideas-ns) contents stream)
  (xpath:with-namespaces (("html" "http://www.w3.org/1999/xhtml"))
    (flet ((get-a-node (path stp)
             (first (xpath:all-nodes (xpath:evaluate path stp))))
           (make-html-node (name)
             (stp:make-element name "http://www.w3.org/1999/xhtml")))
      (let* ((stp (chtml:parse (crlf-to-lf contents) (stp:make-builder)))
             (title (aif+ (get-a-node "//html:h1" stp)
                        (xpath:string-value it) "(Unknown title)"))
             (div (or (get-a-node "//html:div[@class='detail_body']" stp)
                       (error "Couldn't find message body.")))
             (html (make-html-node "html")))
        (let ((head (make-html-node "head" ))
              (title-node (make-html-node "title"))
              (title-text (stp:make-text title))
              (body (make-html-node "body")))
          (stp:append-child title-node title-text)
          (stp:append-child head title-node)
          (stp:append-child html head)
          (stp:append-child html body)
          (stp:detach div)
          (stp:append-child body div))
        (stp:serialize (stp:make-document html)
                       (cxml:make-character-stream-sink stream))
        (values)))))
