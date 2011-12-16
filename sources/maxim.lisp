(in-package :atonews)

;; maxim.com app notes.
(defclass maxim-appnotes-ns (http-source)
  ((list-url :initform
             "http://www.maxim-ic.com/whatsnew/index.cfm/mo/1/sh/app/pl/0")))

(defparameter *maxim-dayblock-regexp*
  (cl-ppcre:create-scanner "BEGIN_DAY_BLOCK: ([0-9]+)-([0-9]+)-([0-9]+)"))

(defparameter *maxim-header-regexp*
  (cl-ppcre:create-scanner
   (concatenate 'string
                "tr class=tablebody>.*?"
                "href=/?([^>]+)>([^<]+).*?"
                "<td>\\s*?([^\\s][^\\n]*)") :single-line-mode t))

(defun maxim-url-to-message-id (url)
  "Produce a message id from the URL (basically, uses the fact that URLs are all
of the form app-notes/index.mvp/id/5030, so just grab the number at the end)"
  (let ((pos (position #\/ url :from-end t)))
    (format nil "<~A@maxim-ic.com>" (subseq url (1+ pos)))))

(defmethod next-message-fragment ((source maxim-appnotes-ns) html pos
                                  current-date-string)
  (let ((next-date (cl-ppcre:scan "BEGIN_DAY_BLOCK" html :start pos))
        (next-title (cl-ppcre:scan "<tr class=tablebody" html :start pos)))
    (when (and current-date-string next-date)
      (setf next-date -1))
    (when next-title
      (block nil
        (when (and next-date (< next-date next-title))
          (multiple-value-bind (start end match-starts match-ends)
              (cl-ppcre:scan *maxim-dayblock-regexp* html :start pos)
            (declare (ignore start))
            (unless end (return (values nil nil nil nil)))
            (setf pos end
                  current-date-string
                  (universal-time-to-2822
                   (apply #'ymd-to-ut
                          (map 'list (lambda (a b)
                                       (parse-integer (subseq html a b)))
                               match-starts match-ends))))))
        (multiple-value-bind (start end match-starts match-ends)
            (cl-ppcre:scan *maxim-header-regexp* html :start pos)
          (declare (ignore start))
          (if (or (not end)
                  (string= "PG" (subseq html
                                        (elt match-starts 1)
                                        (elt match-ends 1))))
              (values nil nil)
              (bind-scan-matches match-starts match-ends
                  (url nil title)
                (values
                 (make-message-fragment
                  (maxim-url-to-message-id url)
                  title "noreply@maxim-ic.com"
                  :date current-date-string
                  :url (concatenate 'string "http://www.maxim-ic.com/" url))
                 end))))))))

(defun matches-before (string regex ending-regex &key (start 0))
  "Return a list of arrays, where you get one for each match for REGEX in STRING
after START, before the first match for ENDING-REGEX."
  (let ((acc))
    (cl-ppcre:do-scans (mt-st mt-e reg-st reg-end regex string nil
                              :start start
                              :end (or (cl-ppcre:scan ending-regex string
                                                      :start start)
                                       (length string)))
      (push (map 'vector (lambda (a b)
                           (subseq string a b))
                 reg-st reg-end)
            acc))
    (nreverse acc)))

(defmethod filter-source-contents ((news-source maxim-appnotes-ns) html stream)
  (format stream "<html>~%<head>~%")
  (let ((sp (make-string-pointer html)))
    (aif+ (search-forward sp "<title>(.*?)</title>")
        (format stream "  <title>~A</title>~%</head>~%~%<body>~%" (aref it 0))
      (error "Could not find title in page source"))

    (unless (search-forward sp "<!--\\s*BEGIN:\\s*TYPE,TITLE,AUTHOR\\s*-->")
      (error "Could not find beginning of content"))

    (aif+ (search-forward sp "grayb5.*\n?\\s*(.*)")
        (format stream "  <h3>~A</h3>~%" (aref it 0))
      (error "Could not find tutorial heading"))

    (aif+ (search-forward sp "<h1>(.*?)\\s*[\\n<]")
        (format stream "  <h1>~A</h1>~%" (aref it 0))
      (error "Could not find H1 title"))

    (when (search-forward sp "<!-- BEGIN: AUTHOR INFO -->" nil)
      (aif+
          (matches-before html
                          "td class=\"grayb5.*\\n?\\s*(.*?)\\s*</td"
                          "END: AUTHOR INFO"
                          :start (pos sp))
          (format stream "  <p>Author~A: ~{~A~^, ~}</p>~%"
                  (if (cdr it) "s" "")
                  (mapcar (lambda (x) (aref x 0)) it))))

    (aif+ (search-forward sp "href=\"(.*\\.pdf)\">Download, PDF Format")
        (format stream "  <p><a href=\"~A\">PDF Version</a></p>~%~%"
                (aref it 0)))

    ;; Go back to the beginning (since the PDF link is miles down)
    (seek sp 0)
    (unless (search-forward sp "<!--\\s*END:\\s*TYPE,TITLE,AUTHOR\\s*-->")
      (error "Can't find the start of the real content"))
    (aif+ (map-find
           (lambda (regex) (cl-ppcre:scan regex html :start (pos sp)))
           '("<!--\\s*BEGIN:\\s*RELATED PARTS\\s*-->"
             "<!--\\s*END:\\s*KINDLE_CONTENT\\s*-->"
             "\\-*<b>The application note you have requested requires"))
        (princ (subseq html (pos sp) it) stream)
      (error "Can't find the end of the real content"))
    (format stream "~%~%</body></html>"))
  "text/html")
