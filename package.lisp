(defpackage :atonews
  (:use :cl))
(in-package :atonews)

;; Needed by both message.lisp and mime.lisp
(defgeneric render-to-message (object stream))
