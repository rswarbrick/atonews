(in-package :atonews)

(defparameter *cmj-maa-root* "maa.publisher.ingentaconnect.com")
(defparameter *cmj-maa-publisher* "cmj@ingentaconnect.com")

(defclass cmj-ns (tiered-source)
  ((list-url :initform (format nil "http://~A/content/maa/cmj" *cmj-maa-root*))))

(defparameter *cmj-issue-regexp*
  (cl-ppcre:create-scanner 
   "href=\"\(/content/maa/cmj/[^;]+\)[^\"]*\".*?>\(.*?\)</a"
   :single-line-mode t)
  "Extracts URL, Title.")

(defparameter *cmj-volume-regexp*
  (cl-ppcre:create-scanner 
   "<li><strong>\(Volume [0-9]+\)</strong></li>"
   :single-line-mode t))

(defun cmj-next-volume (html current-pos)
  "Return the end position of the next volume indicator in HTML and its
title (or nil if none)."
  (multiple-value-bind (start end match-st match-end)
      (cl-ppcre:scan *cmj-volume-regexp* html :start current-pos)
    (declare (ignore start))
    (if end
        (values end (subseq html (aref match-st 0) (aref match-end 0)))
        (values nil nil))))

(defun cmj-next-issue (html current-pos current-volume)
  "Return the end position, URL and title of the next issue. CURRENT-VOLUME
should be the string name of the current volume if we have one."
  (when current-volume
    (multiple-value-bind (start end match-st match-end)
        (cl-ppcre:scan *cmj-issue-regexp* html :start current-pos)
      (declare (ignore start))
      (if (not end)
          (values nil nil nil)
          (bind-scan-matches match-st match-end (url title)
            (values end url (format nil "~A: ~A" current-volume title)))))))

(defun cmj-extract-article-number (url)
  (multiple-value-bind (all matches)
      (ppcre:scan-to-strings
       ".*?/\([0-9]+\)/\([0-9]+\)/\([0-9]+\)/art\([0-9]+\)" url)
    (unless all
      (error "Unexpected issue URL format: ~A" url))
    (apply #'format nil "~A-~A-~A-~A"
           (mapcar #'parse-integer (coerce matches 'list)))))

(defun cmj-make-id (url)
  (format nil "<~A@~A>" (cmj-extract-article-number url) *cmj-maa-root*))

(defun cmj-canonicalise-url (url)
  "Make an absolute url from the relative ones that igentaconnect gives us. Also
strip off the jsessionid rubbish."
  (format nil "http://~A~A"
          *cmj-maa-root*
          (subseq url 0 (position #\; url))))

(defmethod next-issue ((source cmj-ns) html pos current-volume)
  (multiple-value-bind (nv-pos nv-title) (cmj-next-volume html pos)
    (multiple-value-bind (ni-pos ni-url ni-title)
        (cmj-next-issue html pos current-volume)
      (cond
        ((not (or nv-pos ni-pos)) (values nil nil))
        ((and nv-title nv-pos (or (not ni-pos) (< nv-pos ni-pos)))
         (next-issue source html nv-pos nv-title))
        (t
         (values (make-message-fragment
                  (cmj-make-id (concatenate 'string ni-url "/art01"))
                  ni-title
                  *cmj-maa-publisher*
                  :url (cmj-canonicalise-url ni-url))
                 ni-pos current-volume))))))

(defparameter *months*
  '("January" "February" "March" "April" "May" "June"
    "July" "August" "September" "October" "November" "December"))

(defun cmj-issue-to-2822 (issue-title)
  "Get an RFC-2822 date from 'Blah blah blah, MonthName 2000'"
  (multiple-value-bind (all matches)
      (ppcre:scan-to-strings ".* \([^ ]+\) \([0-9]+\)$" issue-title)
    (unless all
      (error "Unexpected issue title format: '~A'. Cannot infer date."
             (aref matches 0)))
    (aif+ (position (aref matches 0) *months* :test #'string=)
        (universal-time-to-2822
         (ymd-to-ut (parse-integer (aref matches 1)) (1+ it) 1))
      (error "Unexpected month in issue title: '~A'." (aref matches 0)))))

(defun cmj-issue-page-parse-date (html)
  (let ((sp (make-string-pointer html)))
    (aif+ (search-forward sp "\(Volume [0-9].*[0-9]\)")
        (cmj-issue-to-2822 (aref it 0))
      (error "Can't find date in issue page."))))

(defmethod next-message-fragment ((source cmj-ns) html pos date)
  ;; Ensure we know the date.
  (unless date (setf date (cmj-issue-page-parse-date html)))
  
  ;; Articles are in <div class="data"> tags. Then it's the only <a> (which has
  ;; the name). The pages come next, in the form pp. blah(n). Finally, there is
  ;; the author or authors (either way, they sit in an <em> tag).
  (let ((sp (make-string-pointer html pos))
        url title)

    (when (search-forward sp "<div class=\"data\">" nil)
      (aif+ (search-forward
             sp "<a href=\"\([^\"]+/art[^\"]+\)\".*?title=\"\([^\"]+\)\"")
          (setf url (cmj-canonicalise-url (aref it 0))
                title (aref it 1))
        (error "Could not find link in data div.")))

    ;; I don't want a story for the Full Issue (which hopefully always gets that
    ;; name and comes last).
    (if (or (not url) (string= "Full Issue" title))
        (values nil nil nil)
        (values (make-message-fragment (cmj-make-id url)
                                       title
                                       *cmj-maa-publisher*
                                       :date date
                                       :url url)
                (pos sp) date))))

(defmethod filter-source-contents ((source cmj-ns) html stream)
  (let ((sp (make-string-pointer html))
        doi)
    (format stream "<html><head>~%")
    (aif+ (search-forward sp "<title>Mathematical.*?&nbsp;\(.*?\)</title>")
        (format stream "  <title>~A</title>~%</head>~%~%<body>~%" (aref it 0))
      (error "Couldn't understand title."))

    ;; Ooh! Dublin core. I'm clearly hip and with the HTML elite. This may or
    ;; may not exist. Just like the download link. *sigh*
    (when (search-forward sp "<meta name=\"DC.identifier\"" nil)
      (aif+ (search-forward sp "content=\"info:doi/\([^\"]+\)\"")
          (setf doi (aref it 0))
        (error "Can't find DOI link.")))

    ;; Freely available articles also have a download link.
    (when (search-forward sp "<span class=\"orangebutton\">" nil)
      ;; The download URL is a GET url, but I guess that doesn't really matter:
      ;; we can just copy it verbatim.
      (aif+ (search-forward sp "<a href=\"\([^\"]+\)\"")
          (format stream "<p><a href=\"~A\">Download</a></p>~%~%" (aref it 0))
        (error "Can't find download URL.")))

    ;; There may or may not be an abstract. If there is, it's titled by
    ;; "<p><strong>Abstract:</strong></p>"
    (when (search-forward sp "<p><strong>Abstract:</strong></p>" nil)
      (let ((start (pos sp)))
        (search-forward sp "</div>" nil)
        (format stream "<p><strong>Abstract:</strong></p>~%<p>~A</p>~%~%"
                (subseq html start (- (pos sp) 6)))))

    ;; Insert the DOI here.
    (when doi
      (format stream "<p>DOI: <a href=\"http://dx.doi.org/~A\">~A</a></p>~%"
              doi doi))

    (format stream "~%~%</body></html>"))
  "text/html")
