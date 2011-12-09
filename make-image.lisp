#-sbcl (error "This only works on SBCL.")
(in-package :atonews)

(defun entry-point ()
  ;; Cargo-culted from stumpwm.
  ;; asdf requires sbcl_home to be set, so set it to the value when the image was built
  (sb-posix:putenv (format nil "SBCL_HOME=~A" #.(sb-ext:posix-getenv "SBCL_HOME")))
  (run)
  0)

(progn
  (sb-ext:save-lisp-and-die
   "atonews" :toplevel #'entry-point :executable t))
