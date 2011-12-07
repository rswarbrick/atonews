(defpackage atonews-asd
  (:use :cl :asdf))
(in-package :atonews-asd)

(defsystem atonews
    :depends-on (:usocket :babel :cl-ppcre :cl-base64)
    :components
    ((:file "package")
     (:file "nntp" :depends-on ("package" "message"))
     (:file "message" :depends-on ("package" "encoding" "mime" "util"))
     (:file "encoding" :depends-on ("package" "util"))
     (:file "util" :depends-on ("package"))
     (:file "mime" :depends-on ("package" "util"))
     (:file "news-source" :depends-on ("package"))
     (:file "maxim" :depends-on ("package" "news-source"))))
