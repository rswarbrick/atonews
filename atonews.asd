(defpackage atonews-asd
  (:use :cl :asdf))
(in-package :atonews-asd)

(defsystem atonews
    :depends-on (:usocket :babel :cl-ppcre :cl-base64 :drakma :cl-fad)
    :components
    ((:file "package")
     (:file "nntp" :depends-on ("package" "message"))
     (:file "message" :depends-on ("package" "encoding" "mime" "util"))
     (:file "encoding" :depends-on ("package" "util"))
     (:file "util" :depends-on ("package"))
     (:file "mime" :depends-on ("package" "util"))
     (:file "news-source" :depends-on ("package" "util" "mime" "message"))
     (:file "fs-utils" :depends-on ("package" "util"))
     (:file "maxim" :depends-on ("package" "news-source" "util"))

     (:file "atonews" :depends-on ("package" "fs-utils" "news-source"
                                             "maxim"))))
