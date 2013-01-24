;gimp-layout-1024x768
#S(STUMPWM::GDUMP :NUMBER 2. :NAME "gimp"
   :TREE
   (((#S(STUMPWM::FDUMP :NUMBER 0. :X 0. :Y 0. :WIDTH 265.
         :HEIGHT 384. :WINDOWS nil :CURRENT nil)
      #S(STUMPWM::FDUMP :NUMBER 1. :X 0. :Y 384. :WIDTH 265.
         :HEIGHT 384. :WINDOWS nil :CURRENT nil))
      #S(STUMPWM::FDUMP :NUMBER 2. :X 265. :Y 0. :WIDTH 759.
        :HEIGHT 768. :WINDOWS nil :CURRENT nil)))
    :CURRENT 1.)

;gimp-rules
((#1="gimp" 0. t t :CLASS "Gimp" :INSTANCE "gimp" :ROLE "gimp-toolbox")
 (#1# 1. t t :CLASS "Gimp" :INSTANCE "gimp" :ROLE "gimp-dock")
 (#1# 2. t t :CLASS "Gimp" :INSTANCE "gimp"))

;; courtesy of Peter Seibel
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
       ,@body))

(defmacro program-with-layout (name &key (command (string-downcase (string name)))
                              (props `'(:class ,(string-capitalize command))))
    (with-gensyms (s w h files-path layout rules)
        `(defcommand ,name () ()
            (let* ((,s (current-screen))
                (,w (prin1-to-string (screen-width ,s)))
                (,h (prin1-to-string (screen-height ,s)))
                (,files-path "/home/jli/etc/stumpwm_files/")
                (,layout (concat ,files-path ,command "-layout-" ,w "x" ,h))
                (,rules (concat ,files-path ,command "-rules")))
                (gnew ,command)
                (restore-from-file ,layout)
                (restore-window-placement-rules ,rules)
                (run-or-raise ,command ,props)
                (place-existing-windows))))) ; needed if command was already run

(program-with-layout gimp)

This is the code that I use to put a "gmail notifier"-style indicator in my mode-line. It requires the XMLS and DRAKMA packages; both of these packages are pretty straightforward to download and install.

(asdf:oos 'asdf:load-op '#:drakma) ; http client
(asdf:oos 'asdf:load-op '#:xmls)   ; XML parser
                
(defvar *gmail-cookies* (make-instance 'drakma:cookie-jar)
    "Contains cookies for talking to gmail server")
(defvar *gmail-username* "<my gmail username>"
    "Username for gmail")
(defvar *gmail-password* "<my gmail password>"
    "Password for gmail")
                                                      
(defun ping-gmail ()
    "Checks gmail's atom feed for new messages.  First return value is number of new messages,
     second is a list of (AUTHOR . TITLE) cons cells."
    (when (and *gmail-username* *gmail-password*)
        (multiple-value-bind (response-body response-code)
            (drakma:http-request "https://mail.google.com/mail/feed/atom" :cookie-jar *gmail-cookies*
                :basic-authorization (list *gmail-username* *gmail-password*))
            (if (= 401 response-code)
                :401-unauthorized
                (let* ((feed-tree (xmls:parse response-body))
                    (fullcount-tag (find "fullcount" (xmls:node-children feed-tree)
                        :key 'xmls:node-name :test 'equal)))
                    (assert (string= "feed" (xmls:node-name feed-tree)))
                    (when (and fullcount-tag
                        (stringp (first (xmls:node-children fullcount-tag))))
                        (values (or (read-from-string (first (xmls:node-children fullcount-tag)))
                                0)
                            (loop for child in (xmls:node-children feed-tree)
                                for title-tag = (when (equal (xmls:node-name child) "entry")
                                    (find "title" (xmls:node-children child)
                                        :key 'xmls:node-name :test 'equal))
                                for author-tag = (when (equal (xmls:node-name child) "entry")
                                    (find "author" (xmls:node-children child)
                                        :key 'xmls:node-name :test 'equal))
                                 when (and title-tag author-tag)
                                    collect (cons
                                        (first (xmls:node-children (first (xmls:node-children author-tag))))
                                        (first (xmls:node-children title-tag)))))))))))
 
(defparameter *gmail-show-titles* nil
    "When non-NIL, show the authors and titles whenever new mail arrives.")
(defparameter *gmail-ping-period* (* 2 60 internal-time-units-per-second)
    "Time between each gmail server ping")
 
(defvar *gmail-last-ping* 0
    "The internal time of the latest ping of the gmail server")
(defvar *gmail-last-value* nil
    "The result of the latest ping of the gmail server")
 
(defun format-gmail (stream)
    "Formats to STREAM a string representing the current status of the gmail mailbox.  Uses cached
     values if it is called more frequently than once every *GMAIL-PING-PERIOD*.  When new mail
     arrives, this function will also display a message containing all the current inbox items"
    (when (> (- (get-internal-real-time)
        *gmail-last-ping*)
        *gmail-ping-period*)
        (multiple-value-bind (num-msgs msg-summaries)
            (ping-gmail)
            (when (and *gmail-show-titles*
                num-msgs (> num-msgs 0)
                (or (null *gmail-last-value*)
                    (/= num-msgs *gmail-last-value*)))
                (let ((*timeout-wait* (* *timeout-wait* (min 2 num-msgs)))) ; leave time to read the titles
                    (message "~A"
                        (with-output-to-string (s)
                        (loop for (author . title) in msg-summaries
                            do (format s "~A - ~A~%" author title))))))
            (setf *gmail-last-value* num-msgs
                *gmail-last-ping* (get-internal-real-time))))
    (cond
        ((null *gmail-last-value*)
            (format stream "[mail:???]"))
        ((and (numberp *gmail-last-value*)
            (zerop *gmail-last-value*))
            (format stream "[mail: 0]"))
        ((numberp *gmail-last-value*)
            (format stream "[MAIL:~2D]" *gmail-last-value*))
        (t
            (format stream "[mail:ERR]"))))
 
(setf *screen-mode-line-format* (list "%w   "
    ;; ... some other modeline settings ...
    '(:eval (format-gmail nil))))


This is the code I use in my .stumpwmrc to display the current CPU and I/O load in the mode-line:

(defvar *prev-user-cpu* 0)
(defvar *prev-sys-cpu* 0)
(defvar *prev-idle-cpu* 0)
(defvar *prev-iowait* 0)
                    
(defun current-cpu-usage ()
    "Return the average CPU usage since the last call.
     First value is percent of CPU in use.
     Second value is percent of CPU in use by system processes.
     Third value is percent of time since last call spent waiting for IO (or 0 if not available)."
    (let ((cpu-result 0)
          (sys-result 0)
          (io-result nil))
        (with-open-file (in #P"/proc/stat" :direction :input)
            (if (eq 'cpu (read in))
                (let* ((norm-user (read in))
                    (nice-user (read in))
                    (user (+ norm-user nice-user))
                    (sys (read in))
                    (idle (read in))
                    (iowait (or (ignore-errors (read in)) 0))
                    (step-denom (- (+ user sys idle iowait)
                        (+ *prev-user-cpu* *prev-sys-cpu* *prev-idle-cpu* *prev-iowait*))))
                    (setf cpu-result (/ (- (+ user sys)
                                        (+ *prev-user-cpu* *prev-sys-cpu*))
                                         step-denom)
                          sys-result (/ (- sys *prev-sys-cpu*)
                                        step-denom)
                          io-result (/ (- iowait *prev-iowait*)
                                        step-denom)
                          *prev-user-cpu* user
                          *prev-sys-cpu* sys
                          *prev-idle-cpu* idle
                          *prev-iowait* iowait))
                    (warn "Unexpected header")))
            (values cpu-result sys-result io-result)))
 
(defun format-current-cpu-usage (stream)
    "Formats a string representing the current processor usage to STREAM.
     Arguments are as those to FORMAT, so NIL returns a formatted string and T prints to standard
     output."
    (multiple-value-bind (cpu sys io) (current-cpu-usage)
        (declare (ignore sys))
        (format stream "[cpu:~3D%] [io:~3D%]" (truncate (* 100 cpu)) (if io (truncate (* 100 io)) 0))))
 
(setf *screen-mode-line-format* (list "%w   "
;; ... some other modeline settings ...
'(:eval (format-current-cpu-usage nil))))

Commands for controlling the volume.

I realized that I was only using xmodmap for volume control at this point, and decided to streamline a bit, especially since StumpWM can display arbitrary messages, which is useful to me. So here's a command to do a define-stumpwm-command for every combination amixer channel and volume-change/muting you want, without copying and pasting the same command again and again.

;;; A command to create volume-control commands
(defun def-volcontrol (channel amount)
    "Commands for controling the volume"
    (define-stumpwm-command
        (concat "amixer-" channel "-" (or amount "toggle")) ()
            (echo-string
                (current-screen)
                (concat channel " " (or amount "toggled") " "
                    (run-shell-command
                        (concat "amixer sset " channel " " (or amount "toggle") "| grep '^[ ]*Front'") t)))))

(defvar amixer-channels '("PCM" "Master" "Headphone"))
(defvar amixer-options '(nil "1+" "1-"))

(let ((channels amixer-channels))
    (loop while channels do
        (let ((options amixer-options))
            (loop while options do
                (def-volcontrol (car channels) (car options))
                (setq options (cdr options))))
            (setq channels (cdr channels))))

(define-stumpwm-command "amixer-sense-toggle" ()
    (echo-string
        (current-screen)
        (concat "Headphone Jack Sense toggled "
            (run-shell-command "amixer sset 'Headphone Jack Sense' toggle" t))))

Note: If you want to use the XF86 volume keys, at present you'll need to add these lines to your .stumpwmrc, or you'll get errors.

(define-keysym #x1008ff11 "XF86AudioLowerVolume")
(define-keysym #x1008ff12 "XF86AudioMute")
(define-keysym #x1008ff13 "XF86AudioRaiseVolume")

...and my setup.

(define-key *top-map* (kbd "XF86AudioLowerVolume")   "amixer-PCM-1-")
(define-key *top-map* (kbd "XF86AudioRaiseVolume")   "amixer-PCM-1+")
(define-key *top-map* (kbd "XF86AudioMute")          "amixer-PCM-toggle")

(define-key *top-map* (kbd "C-XF86AudioLowerVolume") "amixer-Master-1-")
(define-key *top-map* (kbd "C-XF86AudioRaiseVolume") "amixer-Master-1+")
(define-key *top-map* (kbd "C-XF86AudioMute")        "amixer-Master-toggle")

(define-key *top-map* (kbd "M-XF86AudioLowerVolume") "amixer-Headphone-1-")
(define-key *top-map* (kbd "M-XF86AudioRaiseVolume") "amixer-Headphone-1+")
(define-key *top-map* (kbd "M-XF86AudioMute")        "amixer-Headphone-toggle")

(define-key *top-map* (kbd "S-XF86AudioMute")        "amixer-sense-toggle")

I use groups a lot so I found I hardly used the stumpwm "select" command because it only works on the current group, so I made a different version that works for all groups in the current screen.

(defun my-global-window-names ()
    "Returns a list of the names of all the windows in the current screen."
    (let ((groups (sort-groups (current-screen)))
          (windows nil))
        (dolist (group groups)
            (dolist (window (group-windows group))
            ;; Don't include the current window in the list
                (when (not (eq window (current-window)))
                    (setq windows (cons (window-name window) windows)))))
        windows))

(defun my-window-in-group (query group)
    "Returns a window matching QUERY in GROUP."
    (let ((match nil)
          (end nil)
          (name nil))
        (dolist (window (group-windows group))
            (setq name (window-name window)
                  end (min (length name) (length query)))
            ;; Never match the current window
            (when (and (string-equal name query :end1 end :end2 end)
                (not (eq window (current-window))))
                (setq match window)
                (return)))
        match))

(define-stumpwm-type :my-global-window-names (input prompt)
    (or (argument-pop input)
        (completing-read (current-screen) prompt (my-global-window-names))))

(define-stumpwm-command "global-select" ((query :my-global-window-names "Select: "))
    "Like select, but for all groups not just the current one."
    (let ((window nil))
        ;; Check each group to see if it's in
        (dolist (group (screen-groups (current-screen)))
            (setq window (my-window-in-group query group))
            (when window
            (switch-to-group group)
            (frame-raise-window group (window-frame window) window)
            (return)))))

This command runs the stumpwm "quit" command, but only if there aren't any windows open.

(define-stumpwm-command "safequit" ()
    "Checks if any windows are open before quitting."
    (let ((win-count 0))
        ;; Count the windows in each group
        (dolist (group (screen-groups (current-screen)))
            (setq win-count (+ (length (group-windows group)) win-count)))

        ;; Display the number of open windows or quit
        (if (= win-count 0)
            (run-commands "quit")
            (message (format nil "You have ~d ~a open" win-count
                (if (= win-count 1) "window" "windows"))))))

This displays a summary of all new mail in your mail spool. You can use it on a one-off basis or on a timer.

(defvar *newmail-timer* nil
    "Runs the mail checker.")

(defvar my-emails nil
    "The previous formatted contents of the mail spool.")

(defun my-mail-popup (emails)
    "Displays a summary of all new email."
    (let ((summary nil))
        ;; Create the text for the summary
        (if emails
            (setq summary (concatenate 'string (format nil "^6*System mailbox (~a)^n~% ~%"
                (length (split-string emails))) emails))
            (setq summary "^6*System mailbox (0)"))
        ;; Display the summary
        (message "~a" summary)))    

(defun my-get-mail ()
    "Returns the formatted contents of the mail spool."
    (let ((mail nil)
        (lines (run-prog-collect-output "/bin/grep" "-e" "^Subject:"
            "/var/spool/mail/USERNAME")))
        (when (not (string= lines ""))
            ;; Split the subjects by newline
            (setq lines (split-string lines))

            ;; Add each of the subject lines
            (dolist (line lines)
                (setq mail (concatenate 'string mail (format nil "~a~%" (subseq line 9))))))

        mail))

(defun my-check-mail ()
    "Displays the mail popup if there's new email in the spool."
    (let ((newmail (my-get-mail)))
        (when (and newmail (not (string= my-emails newmail)))
            (my-mail-popup newmail))
        (setq my-emails newmail)))

(define-stumpwm-command "mail" ()
    "Displays the mail popup."
    (setq my-emails (my-get-mail))
    (my-mail-popup my-emails))

(defun my-stop-newmail-timer ()
    "Stops the newmail timer."
    (ignore-errors
        (cancel-timer *newmail-timer*)))

(defun my-start-newmail-timer ()
    "Starts the newmail timer."
    (my-stop-newmail-timer)
    (setf *newmail-timer* (run-with-timer 10 10 'my-check-mail)))

(define-stumpwm-command "mailstart" ()
    "Starts the newmail timer."
    (my-start-newmail-timer))

(define-stumpwm-command "mailstop" ()
    "Stops the newmail timer."
    (my-stop-newmail-timer))

Fluxbox-style Alt-F# group ("desktop") switching.

(dotimes (i 13) 
    (unless (eq i 0) ; F0 is non-existant and will error. 
        (define-key *top-map* (kbd (format nil "M-F~a" i)) (format nil "gselect ~a" i))))

If you ever forget which key map you're in, this works kind of like emacs' mode-line except as messages.

(defun show-key-seq (key seq val)
    (message (print-key-seq (reverse seq))))
(add-hook *key-press-hook* 'show-key-seq)

Get emms to do its thing:

(defvar *my-emms-bindings*
    '(("n" "emms-next")
      ("p" "emms-previous")
      ("s" "emms-stop")
      ("P" "emms-pause")))

(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "m")
(let ((m (stumpwm:make-sparse-keymap)))
    (map nil #'(lambda (x)
        (stumpwm:define-key m (stumpwm:kbd (car x))
            (concat "exec emacsclient -e '(" (cadr x) ")'")))
    *my-emms-bindings*)
m))
 

Allow windows to get raised even if they are in an inactive window group:

(defun raise-urgent-window-hook (target)
    (gselect (window-group target))
        (really-raise-window target))
(add-hook *urgent-window-hook* 'raise-urgent-window-hook)

Command to swap two windows (exchange their respective frames): invoke the first time to "tag" the current frame and then a second time to swap the tagged frame with the current one.

(defcommand swap-windows (&optional (frame (tile-group-current-frame (current-group)))) ()
    (if *swap-selected-frame*
        (progn
            (let ((win1 (frame-window *swap-selected-frame*))
                  (win2 (frame-window frame)))
                (when win1 (pull-window win1 frame))
                (when win2 (pull-window win2 *swap-selected-frame*)))
            (setf *swap-selected-frame* nil))
    (setf *swap-selected-frame* frame)))

(define-key *root-map*   (kbd "C-s")    "swap-windows")

(define-stumpwm-command "toggle-touchpad" ()
  "Toggle the laptop touchpad on/off.
     Need to have set 'Option SHMConfig' for Synaptics Touchpad
        device in xorg.conf."
          (let ((state (run-shell-command
                "synclient -l | grep TouchpadOff | awk '{ print $3 }'" t)))
                    (case (string= (subseq state 0 1) "1")
                          (t (shell-command "synclient TouchpadOff=0"))
                                (otherwise (shell-command "synclient TouchpadOff=1")
                                         (banish-pointer)))))

(define-key *root-map* (kbd "T") "toggle-touchpad")

;; Turn off Touchpad initially
(shell-command "synclient TouchpadOff=1")

;; Get rid of cursor initially
(banish-pointer)

;(stumpwm::run-shell-command "stalonetray")
;(stumpwm::run-shell-command "nm-applet")
;(stumpwm::run-shell-command "gnome-volume-control-applet")
;(stumpwm::run-shell-command "gnome-power-manager")
;(stumpwm::run-shell-command "system-config-printer-applet")
;(stumpwm::run-shell-command "dropbox start -i")
;(stumpwm::run-shell-command "gnome-power-manager")
; Keep the X cursor out of the way.
; (run-with-timer 5 5 'banish-pointer)
; Change the background and pointer in X
;(run-shell-command "xsetroot -cursor_name left_ptr -gray -fg white -bg black -name root-window")
; (run-shell-command "feh --bg-scale /home/tsp/.wmii-3.5/wallpaper/wmii.jpg")
; (run-shell-command "xsetbg /home/enigma/media/pictures/artwork/vintage_wallpaper_blue.png")
; Run unclutter so the mouse hangs around no longer than needed.
;(run-shell-command "unclutter -idle 1 -jitter 2 -root")
; I use Xscreensaver as a screensaver. The first line makes sure any running Xscreensaver is killed. The second run regardless of the success of the first & starts a background Xscreensaver daemon
;(run-shell-command "xscreensaver-command -exit; killall xscreensaver 2>/dev/null; xscreensaver -no-splash")

#|
    (define-frame-preference "car"
        (0 nil t :restore   "car-dump" :class "Stardict"))

    (define-frame-preference "cadr"
        (0 t t :create   "cadr-dump" :class "URxvt")
        (0 t t :create   "cadr-dump" :class "Emacs")
        (0 t t :create   "cadr-dump" :class "Xpdf"))

    (define-frame-preference "pidgin"
        (2 t t :create   "pidgin-dump"  :class "Pidgin" :instance "pidgin" :title nil :role nil)
        (0 t t :create   "pidgin-dump"  :class "Pidgin" :instance "pidgin" :title nil :role "conversation")
        (1 t t :create   "pidgin-dump"  :class "Pidgin" :instance "pidgin" :title nil :role "buddy_list"))

    (define-frame-preference "gimp"
        (1 t t :create   "gimp-dump"  :class "Gimp" :title nil :role nil)
        (0 t t :create   "gimp-dump"  :class "Gimp" :title nil :role "gimp-toolbox")
        (2 t t :create   "gimp-dump"  :class "Gimp" :title nil :role "gimp-dock")
        (1 t t :create   "gimp-dump"  :class "Gimp" :title nil :role "gimp-image-window"))
|#
#|
;(replace-hook *mode-line-click-hook*
;    (lambda (mode-line button x y) (grouplist)))

;; display the key sequence in progress
#|
(defun key-press-hook (key key-seq cmd)
    (declare (ignore key))
    (unless (eq *top-map* *resize-map*)
        (let ((*message-window-gravity* :bottom-right))
              (message "Key sequence: ~a" (print-key-seq (reverse key-seq))))
                  (when (stringp cmd)
                        ;; give 'em time to read it
                              (sleep 1.0))))
|#
;(replace-hook *key-press-hook* 'key-press-hook)
;(remove-hook *key-press-hook* 'key-press-hook)

;(clear-window-placement-rules)

;(frame-number raise lock &key create restore dump-name class instance type role title)
;(lock AND raise == jumpto)
;(define-frame-preference "Default"
;    (0 t t :class "XTerm")
;    (1 t t :class "Opera"))

;;Do some key re-mapping; it is crucial that this get run first, because otherwise
;;the remapping later on of Insert and less to the prefix key simply will not work.
;(run-shell-command "xmodmap -quiet ~/.Xmodmap")

;;Apparently modifies some low-level GUI bits of X.
;(run-shell-command "xrdb -load ~/.Xresources -quiet")

;;Change the background and pointer in X
;(run-shell-command "xsetroot -cursor_name left_ptr -gray -fg darkgreen -bg black -name root-window")
