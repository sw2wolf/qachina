#|
;;; to start swank
;(asdf:oos 'asdf:load-op :swank)

(load "/home/friedel/lib/emacs/slime/swank-loader.lisp")
(swank-loader:load-swank)
(swank:create-server :port swank::default-server-port
                     :style swank:*communication-style*
                     :coding-system "utf-8-unix"
                     :dont-close t)
;; Then call slime-connect with defaults from GNU Emacs
;; added swank connection to stumpwm-command "swank"
;; (defcommand swank () ()
;;   (setf stumpwm:*top-level-error-action* :break)
;;   (and (swank:create-server :port swank::default-server-port
;;                             :style swank:*communication-style*
;;                             :coding-system "utf-8-unix"
;;                             :dont-close t)
;;        (echo-string (current-screen) "Starting Swank...Done")))
;;
(defvar *swank-guard* nil)
(unless *swank-guard*
  (setf *swank-guard* t)
    (swank))

(ql:quickload "swank") 
(ql:quickload "quicklisp-slime-helper") 

Paste the following into your dot-emacs 
(load (expand-file-name "/path/to/slime-helper.el")) 
(setq inferior-lisp-program "sbcl") 

Now put the following into your .stumpwmrc or just eval during your stumpwm-session 
(require 'swank) 
(swank:create-server) 
Connect Emacs/Slime to your stumpwm-session using 
M-x slime-connect
|#
