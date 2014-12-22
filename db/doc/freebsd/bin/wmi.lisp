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
	(format t "1:stumpwm-clisp~%")
	(format t "2:stumpwm-ccl~%")
	(format t "3:dwm-dbg~%")
	(format t "9:console~%")
	(format t "----------------------~%")
	(case (read-char-with-timeout nil 5)
		((#\1) (shell "xinit clisp &"))
		((#\2) (shell "xinit ccl &"))
		((#\3) (shell "xinit dwm &"))
		((#\9) (format t "welcome..."))
		(otherwise (shell "xinit &"))))

(main)
(quit)
