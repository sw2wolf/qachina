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
    (format t "please choose WM(1/2/3)~%")
    (format t "----------------------~%")
	(format t "1:stumpwm-ccl with MPD~%")
	(format t "2:stumpwm-clisp with MPD~%")
	(format t "3:xmonad~%")
	(format t "4:dwm~%")
	(format t "9:console~%")
	(format t "----------------------~%")
	(case (read-char-with-timeout nil 5)
		((#\1) (shell "sh -c 'xinit ccl&'"))
		((#\2) (shell "sh -c 'xinit clisp&'"))
		((#\3) (shell "sh -c 'xinit xmonad&'"))
		((#\4) (shell "sh -c 'xinit dwm&'"))
		((#\9) (format t "welcome..."))
		(otherwise (shell "sh -c 'xinit&'"))))

(main)
