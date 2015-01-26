(eval-after-load "erc"
 '(progn
    ;; Basic erc setup
    (setq erc-nick "zRecursive"
	   erc-away-nickname "zRecursive{away}"
       erc-button-buttonize-nicks nil
       erc-user-full-name "zRecursive"
       erc-email-userid "czsq888@163.com"
	   erc-server "irc.freenode.net"
       erc-port "6667"

       erc-autojoin-channels-alist 
	   '(("freenode.net" "#openbsd" "#stumpwm" "#lisp")
		  ("oftc.net" "#suckless" "#luakit"))

       erc-keywords '()
;Hindley is one of the people responsible for Haskell's type system
;rhickey is creator of Clojure
;don stewart, a influential haskeller
	   erc-pals '("Hindley" "rhickey" "dons")

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
;
;telnet irc.freenode.net 6667
;218.100.43.174 91.217.189.44
;
(defun erc-start ()
  (interactive)
  (erc :server "irc.freenode.net" :port 6667 :nick "zRecursive"  :password "zerc999"))

(global-set-key "\C-c\C-e" 'erc-start)

;IDK:     I Don't Know.
;IIRC:    If I Recall Correct
;AFAIK:   As Far As I Know
;IME:     In My Experience
;PITA:    Pain-In-The-Ass

;; /leave	离开
;; /join #channel-name	加入某channel
;; /quit [退出的原因]	退出
;; /away [暂时离开的原因]	不带参数的/away可以解除离开状态
;; /disconnect	强制断开与服务器的连接，与quit的区别在于强制断开，不向服务器发送断开请求
;; /nick 新名字	改名字
;; /me ACTION	产生一条语句：你的名字 ACTION
;; /describe 某人 ACTION	产生一条语句：某人ACTION 像是某人发的
;; /msg 某人 私信内容	私聊(PM=Private Message)，私聊前最好得到允许
;; /names [#channel]	查看某个channel的在线人员
;; /help cmd	查看cmd帮助
;; /whois 名字	常看某人資料
;; /whoami	自己
;; /list	查看所有channel
;; /msg NickServ REGISTER 密码 邮箱  注册
;; /msg NickServ SET PASSWORD 新密码	重新设置密码
