(in-package :atonews)

(defvar *atonews-groups*
  '(("local.electronics.maxim.app-notes" maxim-appnotes-ns)
    ("local.electronics.edn.design-ideas" edn-design-ideas-ns)
    ("local.electronics.analog.cfl" analog-cfl-ns)
    ("local.maths.cmj" cmj-ns)))

(defun run ()
  (with-lock-file
    (let ((server (make-server)))
      (dolist (pair *atonews-groups*)
        (destructuring-bind (name class) pair
          (update-news-source (make-instance class)
                              (make-group name :server server)))))))
