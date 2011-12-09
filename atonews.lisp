(in-package :atonews)

(defun run ()
  (with-lock-file
    (let ((server (make-server)))
      (let ((source (make-instance 'maxim-appnotes-ns))
            (group (make-group "local.electronics.maxim.app-notes" :server server)))
        (update-news-source source group)))))
