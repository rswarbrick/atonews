(defpackage atonews-asd
  (:use :cl :asdf))
(in-package :atonews-asd)

(defsystem atonews
    :depends-on (:usocket :babel :cl-ppcre :cl-base64
                 :drakma :cl-fad :rss :sb-md5 :puri :xmls
                 :closure-html :cxml-stp :xpath)
    :components
    ((:module core
              :pathname ""
              :components
              ((:file "package")
               (:file "nntp" :depends-on ("package" "message"))
               (:file "message" :depends-on ("package" "encoding" "mime" "util"))
               (:file "encoding" :depends-on ("package" "util"))
               (:file "util" :depends-on ("package"))
               (:file "string-pointer" :depends-on ("package"))
               (:file "mime" :depends-on ("package" "util"))
               (:file "news-source" :depends-on ("package" "util" "mime" "message" "nntp"))
               (:file "tiered-source" :depends-on ("news-source"))
               (:file "fs-utils" :depends-on ("package" "util"))
               (:file "pdftotext" :depends-on ("package"))
               (:file "rss" :depends-on ("package" "news-source"))))
     (:module sources
              :depends-on (core)
              :components ((:file "maxim") (:file "edn")
                           (:file "cmj") (:file "analog")))
     (:file "atonews" :depends-on (core sources))))
