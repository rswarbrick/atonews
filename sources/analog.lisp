(in-package :atonews)

(defvar *analog-ns-root* "http://www.analog.com")

(defclass analog-cfl-ns (http-source)
  ((list-url :initform "http://www.analog.com/getAllCircuit.html?locale=en")))

(defun xmlrep-find-child-text (tag treenode)
  "Like xmlrep-find-child-tag, but checks that the resulting node contains a
single child text node and then returns that text."
  (let ((hit (xmls:xmlrep-find-child-tag tag treenode)))
    (unless (and (= 1 (length (cddr hit)))
                 (stringp (third hit)))
      (error "The match for tag <~A> doesn't just contain a text node." tag))
    (third hit)))

(defun analog-cfl-make-url (id)
  (format nil "http://www.analog.com/en/circuits-from-the-lab/~A/vc.html" id))

(defun analog-cfl-parse-circuit (xml)
  (let ((id (xmlrep-find-child-text "circuitID" xml)))
    (make-message-fragment
     (format nil "<cfl-~A@analog.com>" id)
     (xmlrep-find-child-text "description" xml)
     "noreply@analog.com"
     :url (analog-cfl-make-url id))))

(defmethod find-message-fragments ((source analog-cfl-ns) contents)
  (mapcar #'analog-cfl-parse-circuit
          (xmls:xmlrep-find-child-tags "circuit" (xmls:parse contents))))

(defun analog-cfs-dissect-contents (html stream)
  (let ((sp (make-string-pointer html))
        start)
    (format stream "<html><head>~%")
    ;; Conveniently, they give the name without other rubbish as a meta tag
    ;; (keywords).
    (aif+ (search-forward sp "<meta name=\"keywords\" content=\"\([^\"]+\)\"")
        (format stream "  <title>~A</title>~%</head>~%~%<body>~%" (aref it 0))
      (error "Couldn't find keyword <meta> for title."))

    (aif+ (search-forward
           sp "(/static/imported-files/circuit_notes/[^\\.]+\\.pdf)\"")
        (format stream "<p>Download <a href=\"~A~A\">PDF</a></p>~%~%"
                *analog-ns-root* (aref it 0))
      (error "Couldn't find PDF download link."))

    ;; We want the *second* ctfl_left_content_pod div.
    (search-forward sp "class=\"ctfl_left_content_pod" nil)
    (unless (search-forward sp "class=\"ctfl_left_content_pod" nil)
      (error "Can't find content div."))
    ;; Jump backwards to the start of the <div>.
    (seek sp -33 t)
    (unless (string= (subseq html (pos sp) (+ 4 (pos sp))) "<div")
      (error "Backwards jump to start of div failed (can see '~A')"
             (subseq html (pos sp) (+ 4 (pos sp)))))

    (setf start (pos sp))
    
    ;; Jump forwards to the next "ctfl_left_content_pod start" comment
    (unless (search-forward sp "<!-- ctfl_left_content_pod start -->" nil)
      (error "Couldn't find content end comment."))

    (princ "<div>" stream)
    (princ (subseq html start (pos sp)) stream)
    (format stream "~%~%</body></html>")))

(defmethod filter-source-contents ((source analog-cfl-ns) html stream)
  (let* ((dissected (with-output-to-string
                       (string) (analog-cfs-dissect-contents html string)))
         (sp (make-string-pointer dissected))
         (old-point 0))
    ;; We've now extracted the important parts, but need to go through again and
    ;; chop out the lightboxes.
    (loop
       ;; Jump forward to the next one, printing the intervening data.
       (setf old-point (pos sp))
       (aif+ (search-forward sp "largeDiagram_popup" nil)
           ;; 28 is the length of the <div id="...popup string.
           (princ (subseq dissected old-point (- (pos sp) 28)) stream)
         (princ (subseq dissected old-point) stream)
         (return))
       ;; The one we want is called product_large_image
       (unless (search-forward sp "product_large_image" nil)
         (error "Couldn't find relevant div in light box."))
       ;; Now grab the link and the title.
       (let ((url) (title))
         (aif+ (search-forward sp "href=\"([^\" ]+)")
             (setf url (concatenate 'string *analog-ns-root* (aref it 0)))
           (error "Can't find URL for light box."))
         (aif+ (search-forward sp "<strong>([^<]+)")
             (setf title (aref it 0))
           (error "Can't find title for light box."))

         ;; Finally, jump to div_clear and the close to it and the containing
         ;; div.
         (search-forward sp "div_clear" nil)
         (search-forward sp "</div>" nil)
         (search-forward sp "</div>" nil)
         (format stream "<img src=\"~A\">~%<p><strong>~A</strong></p>~%~%"
                 url title)))
    "text/html"))
