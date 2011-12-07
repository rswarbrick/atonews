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

(defun ymd-to-ut (year month date)
  "Convert a YEAR, MONTH, DATE to universal time."
  (encode-universal-time 0 0 0 date month year))

(defun maxim-next-header (html pos current-date-string)
  "Look through the HTML to find the next hit which is either a new date or a
new post. Returns NIL if there are no more, or (VALUES NEW-POS DATE URL TITLE)
if one is found."
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
              (values nil nil nil nil)
              (values
               end current-date-string
               (subseq html (elt match-starts 0) (elt match-ends 0))
               (subseq html (elt match-starts 2) (elt match-ends 2)))))))))

(defun maxim-url-to-message-id (url)
  "Produce a message id from the URL (basically, uses the fact that URLs are all
of the form app-notes/index.mvp/id/5030, so just grab the number at the end)"
  (let ((pos (position #\/ url :from-end t)))
    (format nil "<~A@maxim-ic.com>" (subseq url (1+ pos)))))

(defun maxim-all-headers (html)
  "Find all the note headers in HTML."
  (let ((acc nil) (pos 0) (current-date nil))
    (loop
       (multiple-value-bind (new-pos new-date url title)
           (maxim-next-header html pos current-date)
         (unless new-pos (return))
         (setf pos new-pos
               current-date new-date)
         (push (make-message-fragment
                (maxim-url-to-message-id url)
                title "noreply@maxim-ic.com"
                :date new-date
                :url (concatenate 'string "http://www.maxim-ic.com/" url))
               acc)))
    (nreverse acc)))

(defmethod parse-source-headers ((source maxim-appnotes-ns) contents)
  (maxim-all-headers contents))

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
  (let ((pos 0))
    (flet ((search-forward (regex)
             (multiple-value-bind (start end match-starts match-ends)
                 (cl-ppcre:scan regex html :start pos)
               (declare (ignore start))
               (when end
                 (setf pos end)
                 (map 'vector (lambda (a b) (subseq html a b))
                      match-starts match-ends)))))
      (aif+ (search-forward "<title>(.*?)</title>")
          (format stream
                  "  <title>~A</title>~%</head>~%~%<body>~%" (aref it 0))
        (error "Could not find title in page source"))
      (aif+ (search-forward "<!--\\s*BEGIN:\\s*TYPE,TITLE,AUTHOR\\s*-->")
          nil
        (error "Could not find beginning of content"))
      (aif+ (search-forward "grayb5.*\n?\\s*(.*)")
          (format stream "  <h3>~A</h3>~%" (aref it 0))
        (error "Could not find tutorial heading"))
      (aif+ (search-forward "<h1>(.*?)\\s*[\\n<]")
          (format stream "  <h1>~A</h1>~%" (aref it 0))
        (error "Could not find H1 title"))
      (when (search-forward "<!-- BEGIN: AUTHOR INFO -->")
        (aif+
            (matches-before html
                            "td class=\"grayb5.*\\n?\\s*(.*?)\\s*</td"
                            "END: AUTHOR INFO"
                            :start pos)
            (format stream "  <p>Author~A: ~{~A~^, ~}</p>~%"
                    (if (cdr it) "s" "")
                    (mapcar (lambda (x) (aref x 0)) it))))
      (aif+ (search-forward "href=\"(.*\\.pdf)\">Download, PDF Format")
          (format stream "  <p><a href=\"~A\">PDF Version</a></p>~%~%"
                  (aref it 0)))
      ;; Go back to the beginning (since the PDF link is miles down)
      (setf pos 0)
      (unless (search-forward "<!--\\s*END:\\s*TYPE,TITLE,AUTHOR\\s*-->")
        (error "Can't find the start of the real content"))
      (aif+ (map-find
             (lambda (regex) (cl-ppcre:scan regex html :start pos))
             '("<!--\\s*BEGIN:\\s*RELATED PARTS\\s*-->"
               "\\-*<b>The application note you have requested requires"))
          (princ (subseq html pos it) stream)
        (error "Can't find the end of the real content"))
      (format stream "~%~%</body></html>")))
  "text/html")
