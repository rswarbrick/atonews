(in-package :atonews)

(defclass edn-design-ideas-ns (http-source)
  ((list-url :initform "http://www.edn.com/channel/Design_Ideas.php")))

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

(defmethod next-message-fragment ((source edn-design-ideas-ns) html pos extra)
  (declare (ignore extra))
  (multiple-value-bind (start end match-starts match-ends)
      (cl-ppcre:scan *edn-header-regexp* html :start pos)
    (if (not start)
        (values nil nil)
        (bind-scan-matches match-starts match-ends
            (url title month day year)
          (values (make-message-fragment
                   (edn-make-id url year month day)
                   title "noreply@edn.com"
                   :date (universal-time-to-2822
                          (ymd-to-ut (parse-integer year)
                                     (parse-integer month)
                                     (parse-integer day)))
                   :url (concatenate 'string "http://www.edn.com/" url))
                  end)))))

(defmethod filter-source-contents ((source edn-design-ideas-ns) html stream)
  (let ((sp (make-string-pointer html)))
    (format stream "<html><head>~%")
    (aif+ (or (search-forward sp "<title>(.*?) [^a-z<]*EDN</title>")
              (search-forward sp "<title>(.*)</title>"))
        (format stream "  <title>~A</title>~%</head>~%~%<body>~%"
                (aref it 0))
      (error "Couldn't find a title."))
    (unless (search-forward sp "</h1>" nil)
      (error "Couldn't find start of content"))
    (let ((start (- (pos sp) 4)))
      (unless (search-forward sp "<div class=\"noinfuse\"" nil)
        (error "Couldn't find end of content"))
      (princ (subseq (str sp) start (- (pos sp) 21)) stream))
    (format stream "~%~%</body></html>"))
  "text/html")
