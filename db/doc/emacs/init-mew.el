(setq load-path (cons "/usr/local/share/emacs/site-lisp/mew" load-path))
(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)
(setq mew-icon-directory "/usr/local/share/emacs/site-lisp/mew/etc")
(setq mew-use-cached-passwd t)
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
(setq mew-pop-size 0)
(setq mew-smtp-auth-list nil)
(setq toolbar-mail-reader 'Mew)
(set-default 'mew-decode-quoted 't)  
(when (boundp 'utf-translate-cjk)
       (setq utf-translate-cjk t)
       (custom-set-variables
          '(utf-translate-cjk t)))
(if (fboundp 'utf-translate-cjk-mode)
     (utf-translate-cjk-mode 1)) 

 ;(require 'mew-w3m) ;需要w3m支持，看html邮件
