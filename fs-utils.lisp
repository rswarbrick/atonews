(in-package :atonews)

(defun env-variable-to-directory (variable-name)
  "Get an environment variable that names a directory and return it as a
pathname if it is defined and exists, and NIL otherwise."
  #-SBCL (error "GETENV only sorted on SBCL.")
  #+SBCL (awhen (sb-unix::posix-getenv variable-name)
           (cl-fad:directory-exists-p it)))

(defun directory-from-env-or-default (name default)
  "Either get a directory specified by an environment variable called NAME
or (if either the variable or the directory it specifies doesn't exist) fall
back to DEFAULT (which is resolved below $HOME)."
  (or (env-variable-to-directory name)
      (let ((home (user-homedir-pathname)))
        (or (cl-fad:directory-exists-p (merge-pathnames default home))
            (error "Cannot find a ~A directory (no environment var ~
set and $HOME/~A doesn't exist)"
                   name (namestring default))))))

(defun xdg-data-home ()
  "Return a pathname pointing to an (existing) XDG_DATA_HOME directory."
  (directory-from-env-or-default
   "XDG_DATA_HOME" (make-pathname :directory '(:relative ".local" "share"))))

(defun xdg-config-home ()
  "Return a pathname pointing to an (existing) XDG_CONFIG_HOME directory."
  (directory-from-env-or-default
   "XDG_CONFIG_HOME" (make-pathname :directory '(:relative ".config"))))

(defun ensure-data-dir ()
  "Return a pathname pointing to a suitable data directory. This is created if
necessary."
  (let ((dir (merge-pathnames (make-pathname :directory '(:relative "atonews"))
                              (xdg-data-home))))
    (or (cl-fad:directory-exists-p dir)
        (nth-value 0 (ensure-directories-exist dir)))))

(defun data-file-name (name)
  "Get the pathname for a given data file."
  (merge-pathnames (make-pathname :name name) (ensure-data-dir)))

(defun set-data-file (name object)
  "Set the contents of the data file called NAME in our data dir to be a printed
representation of OBJECT."
  (with-open-file (stream
                   (data-file-name name)
                   :direction :output
                   :if-exists :supersede)
    (let ((*print-readably* t))
      (prin1 object stream))))

(defun read-data-file (name &key (err t) default)
  "Return the contents of the data file called NAME in our data dir. If ERR is
true and the file does not exist, throw an error. Otherwise, if the file does
not exist then return DEFAULT."
  (let ((value default))
    (with-open-file (stream (data-file-name name) :direction :input
                            :if-does-not-exist (and err :error))
      (when stream
        (setf value (read stream nil default))))
    value))

(defun get-config-dir ()
  "Return a pathname pointing to a suitable config directory. This is not
automatically created, so we return NIL if it doesn't exist."
  (cl-fad:directory-exists-p
   (merge-pathnames (make-pathname :directory '(:relative "atonews"))
                    (xdg-config-home))))

(defun rc-file ()
  (awhen (get-config-dir)
    (cl-fad:file-exists-p
     (merge-pathnames (make-pathname :name "atonews.lisp") it))))

(defvar *lock-file-fd* nil)

(defun lock-file ()
  (merge-pathnames (make-pathname :name "lockfile") (ensure-data-dir)))

(defun try-lock-file? ()
  "Returns T and acquires the lockfile (locking it) if possible. Otherwise
returns NIL."
  (or (and *lock-file-fd* t)
      (handler-case
          (locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
            (setf *lock-file-fd*
                  (sb-posix:open (lock-file)
                                 (logior sb-posix:o-wronly sb-posix:o-creat)
                                 (logior sb-posix:s-irusr sb-posix:s-iwusr)))
            (= 0
               (sb-posix:fcntl *lock-file-fd* sb-posix:f-setlk
                               (make-instance 'sb-posix:flock
                                              :type sb-posix:f-wrlck
                                              :whence sb-posix:seek-set
                                              :start 0
                                              :len 1))))
        ;; Catch things like trying to create a file we're not allowed to create
        ;; and the like.
        (sb-posix:syscall-error (err)
          (declare (ignore err))
          (release-lock-file)
          nil))))

(defun release-lock-file ()
  (when *lock-file-fd*
    (sb-posix:close *lock-file-fd*)
    (setf *lock-file-fd* nil))
  (values))

(defmacro with-lock-file (&body body)
  "Lock the lockfile and run BODY. Throws an error if it can't acquire the
lock."
  `(unwind-protect
        (aif+ (not (try-lock-file?))
            (error "Could not acquire lock file.")
          ,@body)
     (release-lock-file)))
