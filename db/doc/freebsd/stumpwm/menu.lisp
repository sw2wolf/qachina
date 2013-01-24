#|
(defparameter *start-menu*
    '( ("Internet"
            ("Web"   "browse")
            ("WinXP" "winxp")
            ("Go"    "eweiqi"))

       ("Graphics"
            ("gqview"   "gqview")
            ("PDF"      "evince"))
       
       ("System Management"
            ("reboot"  "reboot")
            ("halt"    "halt"))))

(defcommand menu () ()
    "docstring"
    (labels ((pick (options)
        (let ((selection (stumpwm::select-from-menu (current-screen) options "")))
            (cond
                ((null selection)
                    (throw 'stumpwm::error "Abort."))
                ((stringp (second selection))
                    (second selection))
            (t
                (pick (cdr selection)))))))
        (let ((choice (pick *start-menu*)))
            (run-shell-command choice))))
(define-key *root-map* (kbd "p") "menu")
|#
