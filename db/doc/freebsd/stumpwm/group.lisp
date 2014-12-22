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

(defun find-group-by-name (name)
 (find name (screen-groups (current-screen)) :test #'string-equal :key #'group-name))

(defun place-windows-on-group (group &key class)
  (lambda (win)
     (when (string-equal (window-class win) class)
          (move-window-to-group win group))))

(defun place-windows-on-frame (group frame &key class)
    (lambda (win)
        (when (string-equal (window-class win) class)
            (move-window-to-group win group)
            (setf (window-frame win) frame)
;      (sync-frame-windows group frame)
            (echo-string (current-screen) (format nil "Window ~a placed in group ~a" (window-name win) (group-name group))))))

(defun horiz-split-frame-and-resize (group fraction)
    (horiz-split-frame group)
    (let ((frame (tile-group-current-frame group)))
        (resize-frame group
            frame
            (truncate (* (- (* 2 fraction) 1) (frame-width frame)))
            'width)))

(defun vert-split-frame-and-resize (group fraction)
    (vert-split-frame group)
    (let ((frame (tile-group-current-frame group)))
        (resize-frame group
            frame
            (truncate (* (- (* 2 fraction) 1) (frame-height frame)))
            'height)))

(defmacro horizontally (&rest frame-specs)
    `(let ((accum 1)
        (frame-specs (sort ',frame-specs (lambda (el1 el2)
            (cond
                ((not (numberp el1)) nil)
                ((not (numberp el2)) t))))))
        (dolist (frame-spec (butlast frame-specs))
            (destructuring-bind (fraction window-queries) frame-spec
                (when (numberp fraction)
                    (decf accum fraction))
                (horiz-split-frame-and-resize group (if (numberp fraction) fraction accum))
                (dolist (window-query window-queries)
                    (ecase (car window-query)
                        (:class (add-hook *map-window-hook*
                            (place-windows-on-frame group (tile-group-current-frame group) :class (cadr window-query)))))))
            (focus-frame-sibling group))
        (destructuring-bind (fraction window-queries) (car (last frame-specs))
            (declare (ignore fraction))
                (dolist (window-query window-queries)
                    (ecase (car window-query)
                        (:class (add-hook *map-window-hook* (place-windows-on-frame group
                            (tile-group-current-frame group)
                            :class (cadr window-query)))))))))

(defmacro define-group-layout (group-name layout-spec)
    `(let* ((group (or (find-group-by-name ,group-name)
        (add-group (current-screen) ,group-name))))
        ,layout-spec))
;(define-group-layout "Chat"
;  (horizontally
;     (1/4 ((:class "Amsn")))
;     (:fill ((:class "Chatwindow")))))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ;;EOF
