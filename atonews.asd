(defpackage atonews-asd
  (:use :cl :asdf))
(in-package :atonews-asd)

(defsystem atonews
    :depends-on (:usocket :babel)
    :components
    ((:file "package")
     (:file "nntp" :depends-on ("package" "message"))
     (:file "message" :depends-on ("package" "encoding"))
     (:file "encoding" :depends-on ("package" "util"))
     (:file "util" :depends-on ("package"))))
