#!/bin/sh
#|
exec clisp -q -q -modern -ansi -norc $0 ${1+"$@"}
exit
|#

(defun read-char-with-timeout (stream timeout)
    (loop with beg = (get-universal-time)
       until (or (listen stream) (< timeout (- (get-universal-time) beg)))
       do (sleep 0.01)
       finally (if (listen stream)
                   (return-from read-char-with-timeout (read-char stream))
                   nil)))

(defun main ()
    (format t "please choose WM(1/2)~%")
    (format t "----------------------~%")
	(format t "1:stumpwm-clisp with MPD~%")
	(format t "2:xmonad~%")
	(format t "3:dwm~%")
	(format t "9:console~%")
	(format t "----------------------~%")
	(case (read-char-with-timeout nil 5)
		((#\1) (shell "xinit clisp"))
		((#\2) (shell "xinit xmonad"))
		((#\3) (shell "xinit dwm"))
		((#\9) (format t "welcome..."))
		(otherwise (shell "xinit"))))

(main)
(quit)
