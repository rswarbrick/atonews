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
         (push (make-message-fragment (maxim-url-to-message-id url)
                                      title "noreply@maxim-ic.com"
                                      :date new-date)
               acc)))
    (nreverse acc)))

(defmethod parse-source-headers ((source maxim-appnotes-ns) contents)
  (maxim-all-headers contents))
