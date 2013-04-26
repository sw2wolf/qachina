(setq load-path (cons "/usr/local/share/emacs/site-lisp/mew" load-path))
(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)

(setq mew-icon-directory "/usr/local/share/emacs/site-lisp/mew/etc")
(setq mew-use-cached-passwd t)
;; mew-pop-size设置成0时，pop邮件大小没有限制
(setq mew-pop-size 0)  
;; 不删除服务器上的邮件  
(setq mew-pop-delete t)  

(if (boundp 'read-mail-command)
     (setq read-mail-command 'mew))
(autoload 'mew-user-agent-compose "mew" nil t)

(if (boundp 'mail-user-agent)
     (setq mail-user-agent 'mew-user-agent))
(if (fboundp 'define-mail-user-agent)
     (define-mail-user-agent
        'mew-user-agent
        'mew-user-agent-compose
        'mew-draft-send-message
        'mew-draft-kill
        'mew-send-hook))

(setq mew-smtp-auth-list nil)
(setq toolbar-mail-reader 'Mew)
(set-default 'mew-decode-quoted 't)

(when (boundp 'utf-translate-cjk)
       (setq utf-translate-cjk t)
       (custom-set-variables
          '(utf-translate-cjk t)))
(if (fboundp 'utf-translate-cjk-mode)
     (utf-translate-cjk-mode 1)) 

;
;;用w3m来读html格式邮件
;; 
(setq mew-mime-multipart-alternative-list '("Text/Html" "Text/Plain" "*."))
(condition-case nil
  (require 'mew-w3m)
  (file-error nil)) 
(setq mew-use-text/html t)

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
	;; ("z9axis"
    ;;      (mailbox-type          imap)
    ;;      (proto                 "%")
    ;;      (imap-server           "imap.gmail.com")
    ;;      (imap-user             "imapuser@gmail.com")
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
	;; news group comp.lang.lisp
	;; (usenet
    ;;     (mailbox-type         . mbox)
    ;;     (proto                . -)
    ;;     (nntp-server           "news.aioe.org")
    ;;     (nntp-user             nil) ;; should be nil default
    ;;     (nntp-header-only      nil)
    ;;     (nntp-newsgroup        "-comp.lang.lisp")
    ;;     (nntp-size             0)
    ;;     (nntp-msgid-user       "z9axis")
    ;;     (smtp-auth-list        ("PLAIN" "LOGIN" "CRAM-MD5"))
    ;;     (smtp-user             "z9axis@gmail.com")
    ;;     (smtp-server           "smtp.gmail.com")
 
    ;;     (mailinglist
    ;;      (mailbox-type          pop)
    ;;      (proto                 +)
    ;;      (pop-server            "pop.gmail.com")
    ;;      (name                  "z9axis")
    ;;      (user                  "z9axis")
    ;;      (mail-domain           "gmail.com")
    ;;      (pop-auth              pass)
    ;;      (pop-user              "z9axis@gmail.com")
    ;;      (smtp-user             "z9axis@gmail.com")
    ;;      (smtp-server           "smtp.gmail.com")))
))

(setq mew-ssl-verify-level 0)
