#!/bin/sh
#|
exec clisp -q -q -modern -ansi -norc $0 $0 ${1+"$@"}
exit
|#

(load "~/quicklisp/asdf")

(push #p"~/quicklisp/dists/quicklisp/software/cl-ppcre-2.0.4/" asdf:*central-registry*)
(asdf:oos 'asdf:load-op :cl-ppcre)

(push #p"~/quicklisp/dists/quicklisp/software/clx-20121125-git/" asdf:*central-registry*)
(asdf:oos 'asdf:load-op :clx)

(push #p"/media/D/qachina/db/doc/money/" asdf:*central-registry*)
(asdf:oos 'asdf:load-op :money)

(push #p"~/quicklisp/local-projects/stumpwm/" asdf:*central-registry*)
(asdf:oos 'asdf:load-op :stumpwm)

(ext:saveinitmem "stumpwm" :init-function (lambda ()
                                            (stumpwm:stumpwm)
                                            (ext:quit))
     :executable t :keep-global-handlers t :quiet t :norc t)

(quit)
