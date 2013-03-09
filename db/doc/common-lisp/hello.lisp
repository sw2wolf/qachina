;;; -*- Mode:Lisp; Syntax: Common-lisp; Package:XLIB; Base:10; Lowercase: Yes -*-

(in-package :xlib)

(defun input-handle-key-press-event (&rest event-slots &key event-key root code state &allow-other-keys)
  (declare (ignore event-slots root))
  ;; FIXME: don't use a cons
  (list* event-key code state))

(defun input-handle-selection-event (&key window selection property &allow-other-keys)
  (declare (ignore selection))
  (if property
      (xlib:get-property window property :type :string :result-type 'string :transform #'xlib:card8->char :delete-p t)
      ""))

(defun read-key-or-selection-handle-event (&rest event-slots &key display event-key &allow-other-keys)
  (declare (ignore display))
  (stumpwm::message-no-timeout "slots:~a~%" event-slots)
  (case event-key
    ((or :key-release :key-press)
     (apply 'input-handle-key-press-event event-slots))
    (:selection-notify
     (apply 'input-handle-selection-event event-slots))
    (t nil)))

(defun read-key-or-selection (display)
  (loop for ev = (xlib:process-event display :handler #'read-key-or-selection-handle-event :timeout nil) do
	   ;(stumpwm:message "event: ~a~%" ev)
       (cond ((stringp ev)
              (return ev))
             ((and (consp ev)
                   (eq (first ev) :key-press))
              (return (cdr ev))))))

(defun hello-world (host &rest args &key (string "Hello World") (font "fixed"))
  ;; CLX demo, says STRING using FONT in its own window on HOST
  (let ((display nil)
	(abort t))
    (unwind-protect
	(progn 
	  (setq display (open-display host))
	  (multiple-value-prog1
	    (let* ((screen (display-default-screen display))
		   (black (screen-black-pixel screen))
		   (white (screen-white-pixel screen))
		   (font (open-font display font))
		   (border 1)			; Minimum margin around the text
		   (width (+ (text-width font string) (* 2 border)))
		   (height (+ (max-char-ascent font) (max-char-descent font) (* 2 border)))
		   (x (truncate (- (screen-width screen) width) 2))
		   (y (truncate (- (screen-height screen) height) 2))
		   (window (create-window :parent (screen-root screen)
					  :x x :y y :width width :height height
					  :background black
					  :border white
					  :border-width 1
					  :colormap (screen-default-colormap screen)
					  :bit-gravity :center
					  :event-mask '(:exposure :button-press :key-press :key-release)))
		   (gcontext (create-gcontext :drawable window
					      :background black
					      :foreground white
					      :font font)))
	      ;; Set window manager hints
	      (set-wm-properties window
				 :name 'hello-world
				 :icon-name string
				 :resource-name string
				 :resource-class 'hello-world
				 :command (list* 'hello-world host args)
				 :x x :y y :width width :height height
				 :min-width width :min-height height
				 :input :on :initial-state :normal)
	      (map-window window)		; Map the window
	      ;; Handle events
	      (event-case (display :discard-p t :force-output-p t)
		(exposure  ;; Come here on exposure events
		  (window count)
		  (when (zerop count) ;; Ignore all but the last exposure event
		    (with-state (window)
		      (let ((x (truncate (- (drawable-width window) width) 2))
			    (y (truncate (- (+ (drawable-height window)
					       (max-char-ascent font))
					    (max-char-descent font))
					 2)))
			;; Draw text centered in widnow
			(clear-area window)
			(draw-glyphs window gcontext x y string)))
		    ;; Returning non-nil causes event-case to exit
		    nil))
		(button-press () t)
		(key-release () (read-key-or-selection display) nil)))
	    (setq abort nil)))
      ;; Ensure display is closed when done
      (when display
	(close-display display :abort abort)))))
