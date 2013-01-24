#|
(defun swap-groups (group1 group2)
    (rotatef (slot-value group1 'number) (slot-value group2 'number)))

(defun move-group-forward (&optional (group (current-group)))
    (swap-groups group (next-group group (sort-groups (current-screen)))))

(defun move-group-backward (&optional (group (current-group)))
    (swap-groups group (next-group group (reverse (sort-groups (current-screen))))))

(define-stumpwm-command "gforward" ()
    (move-group-forward)
    (echo-groups (current-screen) *group-format*))

(define-stumpwm-command "gbackward" ()
    (move-group-backward)
    (echo-groups (current-screen) *group-format*))
|#
#|
;; Modeline Group Scrolling - make sure *screen-mode-line-format* has a "%g"
(defun mode-line-click-hook (ml button x y)
    (declare (ignore ml y))
    ;(run-shell-command (format nil "notify-send 'button:~d x:~d y:~d'" button x y))
    (cond ((>= x 400)
        (cond ((eq button 5)
               (run-commands "next"))
              ((eq button 4)
               (run-commands "prev"))))
        (t
            (cond ((eq button 5)
                   (run-commands "gnext"))
                  ((eq button 4)
                    (run-commands "gprev"))))))

(add-hook *mode-line-click-hook* 'mode-line-click-hook)
|#
