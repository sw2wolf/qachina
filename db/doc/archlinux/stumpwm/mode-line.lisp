#|
(defun update-mode-line () "Update the mode-line sooner than usual."
    (let ((screen (current-screen)))
        (when (screen-mode-line screen)
            (redraw-mode-line-for (screen-mode-line screen) screen))))

;;SBCL-specific
;;Call update-mode-line in such and such a way. Borrowed from Luigi Panzeri.
#+sbcl
(defparameter *mode-line-timer* (sb-ext:make-timer
    #'update-mode-line
    :name "mode-line-updating"
    :thread (car (last (sb-thread:list-all-threads)))))
;;Call a function given by the parameter every 30 seconds. This will increment the stuff in the mode-line.
#+sbcl (sb-ext:schedule-timer *mode-line-timer* 5 :repeat-interval 60 :absolute-p nil)
|#
