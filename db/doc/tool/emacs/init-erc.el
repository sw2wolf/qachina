(eval-after-load "erc"
  '(progn
     ;; Basic erc setup
     (setq erc-nick "sw2wolf"
		   erc-away-nickname "sw2wolf{away}"
	       erc-button-buttonize-nicks nil
           erc-user-full-name "sw2wolf"
		   erc-email-userid "czsq888@163.com"
		   erc-server "irc.freenode.net"
		   erc-port "6667"

           erc-autojoin-channels-alist 
		   '(("freenode.net" "#openbsd" "#erlang" "#ocaml")
			 ("oftc.net" "#emacs-cn"))

           erc-keywords '("lisp" "racket" "haskell")
;Hindley is one of the people responsible for Haskell's type system
		   erc-pals '("rms" "Hindley" "Anniepoo" "ski")

           erc-format-nick-function 'erc-format-@nick
           erc-interpret-mirc-color t
           erc-button-buttonize-nicks nil
           erc-track-position-in-mode-line 'after-modes)

	 (setq erc-default-coding-system '(utf-8 . utf-8))
	 (setq erc-encoding-coding-alist '(("#linuxfire" . chinese-iso-8bit)))
     (setq erc-ignore-list nil)

     (erc-scrolltobottom-enable)
     ;(erc-spelling-mode t)
     (erc-netsplit-mode t)
     (erc-autojoin-mode 1)
	 (erc-match-mode 1)

     ;; Lots of default messages say the whole hostname of a user. Instead, use
     ;; short forms.
     (erc-define-catalog-entry 'english 'JOIN
                               "%n has joined channel %c")
     (erc-define-catalog-entry 'english 'NICK
                               "%n is now known as %N")
     (erc-define-catalog-entry 'english 'MODE
                               "%n has change mode for %t to %m")
     (erc-define-catalog-entry 'english 'QUIT
                               "%n has quit: %r")
     (erc-define-catalog-entry 'english 'TOPIC
                               "%n has set the topic for %c: \"%T\"")

	 (require 'erc-ring)
	 (erc-ring-enable)

     (require 'erc-goodies)

     ;; Don't spam me bro
     (setq erc-hide-list '("JOIN" "PART" "QUIT"))

     ;; Don't spam my modeline.
     (require 'erc-track)
     (erc-track-mode 1)
     (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                     "324" "329" "332" "333" "353" "477"))
	 ;; Don't track server buffer
	 (setq erc-track-exclude-server-buffer t)

;; Kill buffers for channels after /part
     ;(setq erc-kill-buffer-on-part t)
     ;; Kill buffers for private queries after quitting the server
     ;(setq erc-kill-queries-on-quit t)
     ;; Kill buffers for server messages after quitting the server
     ;(setq erc-kill-server-buffer-on-quit t)

;; Logging
	 ;; (setq erc-log-channels t
	 ;; 	   erc-log-channels-directory "~/logs/erc"
	 ;; 	   erc-log-insert-log-on-open nil
	 ;; 	   erc-log-file-coding-system 'utf-8)

     ;; Nickserv
     ;(load "~/.elliot-unix/emacs/erc-auth")
     ;(setq erc-prompt-for-nickserv-password nil)

     ;(require 'erc-services)
     ;(erc-services-mode 1)
	 (add-hook 'erc-after-connect
    	  '(lambda (SERVER NICK)
    	     (cond
    	      ((string-match "freenode\\.net" SERVER)
    	       (erc-message "PRIVMSG" "NickServ identify password1"))
    
    	      ((string-match "oftc\\.net" SERVER)
    	       (erc-message "PRIVMSG" "NickServ identify password2"))
    
    	      ((string-match "jin\\.tekken" SERVER)
    	       (erc-message "PRIVMSG" "#bitlbee identify password3")))))

     ;; Truncate buffers so they don't hog core.
     (setq erc-max-buffer-size 20000)
     (defvar erc-insert-post-hook)
     (add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
     (setq erc-truncate-buffer-on-save t)))

(defun erc-start ()
  (interactive)
  ;(erc-ssl :server "irc.oftc.net" :port 6697 :nick erc-nick :password oftcpw)
  (erc :server "irc.freenode.net" :port 6667 :nick "sw2wolf" :password "zerc999"))
(global-set-key "\C-c\C-e" 'erc-start)
