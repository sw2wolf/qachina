(add-to-list 'load-path (expand-file-name "~/.emacs.d/mew-6.6"))
(setq mew-icon-directory "~/.emacs.d/mew-6.6/etc")

(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)
(autoload 'mew-user-agent-compose "mew" nil t)

;(setq mew-debug t)

;;设定将密码保存一段时间，默认20分钟 
(setq mew-use-cached-passwd t)
(setq mew-passwd-timer-unit 999)
(setq mew-passwd-lifetime 999)

; external programs
;(setq mew-prog-pgp "gpg") ; (使用gpg加密邮件密码)
;(setq mew-prog-ssl "/usr/local/bin/stunnel") ; stunnel路径

; charset
(setq mew-charset-m17n "utf-8")
(setq mew-internal-utf-8p)

;; mew-pop-size设置成0时，pop邮件大小没有限制
(setq mew-pop-size 0)  

;; 删除服务器上的邮件  
(setq mew-pop-delete t)  

(if (boundp 'read-mail-command)
     (setq read-mail-command 'mew))
(setq read-mail-command 'mew)

(if (boundp 'mail-user-agent)
     (setq mail-user-agent 'mew-user-agent))
(setq mail-user-agent 'mew-user-agent)

(if (fboundp 'define-mail-user-agent)
     (define-mail-user-agent
        'mew-user-agent
        'mew-user-agent-compose
        'mew-draft-send-message
        'mew-draft-kill
        'mew-send-hook))

;;smtp服务器不用认证采用下面设定 
(setq mew-smtp-auth-list nil)

(setq toolbar-mail-reader 'Mew)
(set-default 'mew-decode-quoted 't)

;
;;用w3m来读html格式邮件
;; 
(setq mew-mime-multipart-alternative-list '("Text/Html" "Text/Plain" "*."))
(condition-case nil
  (require 'mew-w3m)
  (file-error nil))
(setq mew-use-text/html t)
(setq mew-use-w3m-minor-mode t)
(setq mew-w3m-auto-insert-image t) ; 调用W3M读取读取HTML邮件，自动显示图片

(setq mew-config-alist '(
    ("default"
         ("name"         .  "z_axis")
         ("user"         .  "z_axis")
         ("smtp-server"  .  "smtp.163.com")
         ("smtp-port"    .  "25")
         ("pop-server"   .  "pop.163.com")
         ("pop-port"     .  "110")
         ("smtp-user"    .  "z_axis")
         ("pop-user"     .  "z_axis")
         ("mail-domain"  .  "163.com")
         ("mailbox-type" .  pop)
         ("pop-auth"     .  pass)
         ("smtp-auth-list" . ("PLAIN" "LOGIN" "CRAM-MD5")))
    ("czsq"
         ("name"         .  "czsq888")
         ("user"         .  "czsq888")
         ("smtp-server"  .  "smtp.163.com")
         ("smtp-port"    .  "25")
         ("pop-server"   .  "pop.163.com")
         ("pop-port"     .  "110")
         ("smtp-user"    .  "czsq888")
         ("pop-user"     .  "czsq888")
         ("mail-domain"  .  "163.com")
         ("mailbox-type" .  pop)
         ("pop-auth"     .  pass)
         ("smtp-auth-list" . ("PLAIN" "LOGIN" "CRAM-MD5")))
	("midas"
         ("name"         .  "midas")
         ("user"         .  "midas")
         ("smtp-server"  .  "smtp.163.com")
         ("smtp-port"    .  "25")
         ("pop-server"   .  "pop.163.com")
         ("pop-port"     .  "110")
         ("smtp-user"    .  "midas_z")
         ("pop-user"     .  "midas_z")
         ("mail-domain"  .  "163.com")
         ("mailbox-type" .  pop)
         ("pop-auth"     .  pass)
         ("smtp-auth-list" . ("PLAIN" "LOGIN" "CRAM-MD5")))

	;; ("z9axis"
    ;;      (mailbox-type          imap)
    ;;      (proto                 "%")
    ;;      (imap-server           "imap.gmail.com")
    ;;      (imap-user             "z9axis@gmail.com")
    ;;      (name                  "z9axis")
    ;;      (user                  "z9axis")
    ;;      (mail-domain           "gmail.com")
    ;;      (imap-size             0)
    ;;      (imap-delete           t)
    ;;      (imap-queue-folder     "%queue")
    ;;      (imap-trash-folder     "%Trash") ;; This must be in concile with your IMAP box setup
    ;;      (smtp-auth-list        ("PLAIN" "LOGIN" "CRAM-MD5")) 
    ;;      (smtp-user             "z9axis@gmail.com")
    ;;      (smtp-server           "smtp.gmail.com"))
))

;; (setq mew-proto "%") ; 邮箱头符号
;; (setq mew-from "z9axis@gmail.com")
;; (setq mew-imap-server "imap.gmail.com")
;; (setq mew-imap-user "z9axis")
;; (setq mew-imap-auth  t) ; 启用IMAP身份验证
;; (setq mew-imap-ssl t) ; 启动IMAP加密链接
;; (setq mew-imap-ssl-port "993") ; IMAP加密链接端口号
;; (setq mew-smtp-auth t) ; 启用SMTP身份验证
;; (setq mew-smtp-ssl t) ; 启用SMTP加密链接
;; (setq mew-smtp-ssl-port "465") ; SMTP加密链接端口号
;; (setq mew-smtp-server "smtp.gmail.com")
;; (setq mew-smtp-user "z9axis@gmail.com")
;; (setq mew-fcc "%Sent Items") ; 发送文件抄送到%Sent Items目录
;; (setq mew-imap-trash-folder "%Trash") ; 删除邮件到%Trash目录

(setq mew-ssl-verify-level 0)

;;;;;;;;;;;;;;;;;;;;;; 
;;信件引用设置 
;;;;;;;;;;;;;;;;;;;;;; 
(setq mew-cite-fields '("From:" "Subject:" "Date:" "Message-ID:")) 
(setq mew-cite-format "From: %s\nSubject: %s\nDate: %s\nMessage-ID: %s\n\n") 
(setq mew-cite-prefix-function 'mew-cite-prefix-username) 

;; mew 默认使用的邮件签名档位于 ~/.signature 文件，由变量 mew-signature-file 控制，快捷键 ‘C-c TAB’ 会在光标处插入签名档。建议将签名档放置在邮件的末尾，可以通过 mew-signature-as-lastpart’ 与 ‘mew-signature-insert-last’设置。如果需要在撰写或是回复邮件时，需要自动插入签名档，可以通过 mew-draft-mode-newdraft-hook 进行插入。
(setq mew-signature-file "~/Mail/signature")
(setq mew-signature-as-lastpart t)
(setq mew-signature-insert-last t)
(add-hook 'mew-before-cite-hook 'mew-header-goto-body)
(add-hook 'mew-draft-mode-newdraft-hook 'mew-draft-insert-signature)

;; 如果为设置任何的 refile rules，在 summary-mode 里面按 o 即可对当前邮件进行分类，mew 会问你把邮件分类到哪个文件夹里面去，并提供了一个默认的选项，通常情况下默认选项就是正确的选项，所以直接回车就可以了。
;; 虽然 mew 的 refil-by-guess 很方便，但是更加使用的还是自定义 refile rules。mew 提供的 refile 非常丰富，但感觉实用可定性好的还是 Guess by user defined rules。例如，如果你订阅了 octave，freebsd 的帮助邮件列表，很自然的会需要把此三个邮件列表的邮件分开存放在不同的本地文件下，而通常的邮件会存放到 inbox folder。相应的只需要把邮件头的 To 和 Cc 域里包含有 “@octave.org”的邮件 refile 到 +math/octave 文件夹中，同时把 To 和 Cc 域里含有”@freebsd.org”的邮件 refile 到 +unix/freebsd，就可以使用如下的实例代码
;; 完成 refile rules 后，在收取邮件后，按下 ‘M-o’ 即可对当前 mode 下的邮件按照定义的 rules 进行 refile。然后，你想要查阅 octave 邮件列表，只需要按下 ‘g’ 与 ‘+math/octave’，就可以切换到 octave 文件夹，此时再按下 ‘tt’，既可以进入 mew 提供的 thread 功能，此时的 mode 称为 virtual folder mode，mew 会指示出各个邮件之间的层次关系，非常方便。
;; 如果只使用小写字母命名自己的邮件文件夹的话，可以把“mew-use-fast-refile” 设置为 t 用于加快速度。
;; (setq mew-refile-guess-alist
;;   '(("To:"
;; 	 ("@octave.org"			. "+math/octave")
;; 	 ("@freebsd.org"		. "+unix/freebsd"))
;; 	("Cc:"
;; 	 ("@octave.org"			. "+math/octave")
;; 	 ("@freebsd.org"		. "+unix/freebsd"))
;; 	(nil . "+inbox")))
;; (setq mew-refile-guess-control
;;   '(mew-refile-guess-by-folder
;; 	mew-refile-guess-by-alist))

;; 各个 folder 的 summary-mode 显示邮件的样式由 “mew-summary-form’ 控制，通过对此变量的控制，你可以具体定制 summary-mode 显示的邮件域。特别的，summary-mode 中的 from 域还可以由变量 ‘mew-summary-form-extract-rule’进行具体的设置，可以在 ‘nickname”，’name”，’address” 和 ‘comment。自己为了使 summary-mode 显示看起来不是很拥挤，不显示 “Body:” 域，并且增加了 “Subject:” 域，更改 from 仅仅显示寄件人的 name。
(setq mew-summary-form
      '(type (5 date) " " (14 from) " " t (0 subj)))
(setq mew-summary-form-extract-rule '(name))

;; 在设置调整好变量后，你可能会发现 summary-mode 还是没有任何变化，这是由于 mew 的默认缓存机制所致，你可以手动删除缓存文件强制 mew 重新生成。缓存文件位于相应 folder 下，例如：~/Mail/inbox/.mew-summary 。
;; 设置 “mew-summary-form-mark-spam” 为 t 可以在 spam message 前制动添加 D。

;; 默认的邮件保存在~/Mail，里面的文件Addrbook是地址本设置，设置好，在写邮件时可以自动补全，同gnus里使用bbdb一样，也有组群发功能。 

(defvar my-mew-cases '("default" "czsq" "midas"))
(defvar my-mew-orig-case "default")
(defvar my-mew-current-caselist my-mew-cases)

(defun my-mew-summary-set-case (case)
  "Set the case."
  (setq mew-case case)
  (let ((case mew-case)) ;; side effect
    (save-excursion
      (dolist (buf mew-buffers)
        (when (get-buffer buf)
          (set-buffer buf)
          (cond
            ((mew-summary-p)
             (mew-summary-mode-name mew-mode-name-summary))
            ((mew-virtual-p)
             (mew-summary-mode-name mew-mode-name-virtual))))))
    (when mew-visit-inbox-after-setting-case
      (let ((inbox (mew-case-folder
                    case
                    (mew-proto-inbox-folder (mew-proto case) case))))
        (mew-summary-visit-folder inbox)))))

;; (defun my-mew-summary-retrieve-all ()
;;   (interactive)
;;   (setq my-mew-orig-case mew-case)
;;   (my-mew-summary-set-case (car my-mew-cases))
;;   (setq my-mew-current-caselist (cdr my-mew-cases))
;;   (mew-summary-retrieve))

;(define-key mew-summary-mode-map "I"  'my-mew-summary-retrieve-all)

(defadvice mew-net-disp-info-display (after my-cache-save-postfix-action)
  ;(sleep-for 1)
  (cond (my-mew-current-caselist
         (my-mew-summary-set-case (car my-mew-current-caselist))
         (setq my-mew-current-caselist (cdr my-mew-current-caselist))
         (mew-summary-retrieve))
        (my-mew-orig-case
         ;(sleep-for 1)
         (message "retrieve all accounts done.")
         (my-mew-summary-set-case my-mew-orig-case)
         (setq my-mew-orig-case nil))
		(t (setq my-mew-current-caselist my-mew-cases))))

(ad-activate 'mew-net-disp-info-display)

;; (load "biff")
;; (setq mew-use-biff t)
;; (setq mew-use-biff-bell t)
;; (setq mew-biff-interval 150) ；这个值一定要小于下面的timer-unit和lifetime值，这个可以使用describe-variable查看一下
;; (setq mew-pop-biff-interval 3)
;上面是设置biff每隔五分钟自动检查一下邮箱，如果有新邮件，则emacs的状态栏上会有Mail(n)的提示---n表示新邮件数目。有人实现了可以播放其它声音，抄录如下：

;; (setq mew-arrivedmail-pending 0)
;; (defadvice mew-biff-bark (before mew-biff-sound (arg))
;;  "Play a sound, if new Mail arrives"
;;     (cond ((and (> arg 0) (> arg mew-arrivedmail-pending))
;;         (setq mew-arrivedmail-pending arg)
;;         (start-process-shell-command "mail-sound" "*Messages*" "sndplay ~/.elisp/mail.wav"))
;;         ;; replace sndplay with your favorite command to play a sound-file
;;         ((= arg 0)
;;             (if (> mew-arrivedmail-pending 0)
;;                  (setq mew-arrivedmail-pending 0)))))
;; (ad-activate 'mew-biff-bark)

;; 一个地址簿样本： ~/Mail/Addrbook
;; clisp clisp-list@lists.sourceforge.net
;; qemu qemu-discuss@nongnu.org
