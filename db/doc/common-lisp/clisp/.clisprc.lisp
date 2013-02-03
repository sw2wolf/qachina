;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(defun project-setup ()
    (mapcar #'(lambda (path) (push path asdf:*central-registry*))
        '(#p"/media/D/qachina/db/doc/money/"
          #p"/media/D/qachina/")))

(project-setup)
(ql:quickload :money)
