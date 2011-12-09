(in-package :atonews)

(defun run ()
  (with-lock-file
    (let ((server (make-server)))
      (map nil
           #'update-news-source
           (list (make-instance 'maxim-appnotes-ns)
                 (make-instance 'edn-design-ideas-ns))
           (list (make-group "local.electronics.maxim.app-notes" :server server)
                 (make-group "local.electronics.edn.design-ideas" :server server))))))
