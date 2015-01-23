;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(setf *default-file-character-encoding* :utf-8)
(push :HUNCHENTOOT-NO-SSL *features*)

;(defun setup-registry (directory-path)
    ;(format t "; adding components under ~A to asdf registry~%" directory-path)
;    (mapc (lambda (asd-pathname)
;                (pushnew (make-pathname :name nil :type nil :version nil :defaults asd-pathname) asdf:*central-registry*))
;          (directory (merge-pathnames #p"*/*.asd" directory-path))))

;(setup-registry #p"/media/E/myapp/")
;(setup-registry #p"/media/E/www/qachina/")

;; (pushnew #p"/media/D/qachina/" asdf:*central-registry*)
(pushnew #p"d:/qachina/db/doc/money/" asdf:*central-registry*)

(ql:quickload :money)
