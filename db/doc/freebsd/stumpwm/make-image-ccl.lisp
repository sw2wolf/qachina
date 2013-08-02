#!/bin/sh
#|
exec ~/ccl/fx86cl \
	-e '(set-dispatch-macro-character #\# #\! 
            (lambda (stream subchar arg)
                (declare (ignore subchar arg))
                (read-line stream)
                (values)))' \
    -Q -n -l "$0" -- "$@"
|#

(setf *load-verbose* nil *load-print* nil)
(load "~/quicklisp/asdf")

(push #p"~/quicklisp/dists/quicklisp/software/cl-ppcre-2.0.4/" asdf:*central-registry*)
(asdf:oos 'asdf:load-op :cl-ppcre)

(push #p"~/quicklisp/dists/quicklisp/software/clx-20121125-git/" asdf:*central-registry*)
(asdf:oos 'asdf:load-op :clx)

(push #p"~/quicklisp/local-projects/stumpwm/" asdf:*central-registry*)
(asdf:oos 'asdf:load-op :stumpwm)

(ccl:save-application "stumpwm" :prepend-kernel t :toplevel-function #'stumpwm:stumpwm)

(quit)
