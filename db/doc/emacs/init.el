;default-input-method
;C-x RET C-\ , then choose an input method.
;
(add-to-list 'load-path "~/.emacs.d/")

;disable logging to *Messages*
;(fset 'message 'ignore)
(setq messages-buffer-max-lines nil)

(require 'cl)

(load "init-base")
(load "init-key")

(load "init-eshell")

(load "init-dict")
(load "init-erc")

(load "init-w3m")
(load "init-mew")

;(load "init-ac")

(load "init-scheme")

;(load "init-slime")
;(load "init-clojure")

;(load "init-erlang")
;(load "init-prolog")

;(load "init-haskell")
;(load "init-ocaml")

;(load "init-ruby")
;(load "init-forth")
;(load "init-lua")

;(load "init-git")
;(load "init-sqlite")

;(load "init-emms")
;(load "init-radio")
;(load "init-maxima")

;(load "init-package")

(setq shell-file-name "/usr/local/bin/bash")

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;默认链接网络浏览器打开
(setq browse-url-generic-program (executable-find "dwb"))
(setq browse-url-browser-function 'browse-url-generic)
(global-set-key "\C-c\C-g" 'browse-url-at-point)

;; ------------------------------------------------------------ [ ispell ]
(eval-after-load "ispell"
  '(progn
     ;; Use the -C option when running aspell, which will
     ;; ConsiderCamelCaseToBeCorrect
     (setq ispell-extra-args '("-C"))))

(setq initial-frame-alist '((top . 0) (left . 0) (width . 1024) (height . 768)))
(add-hook 'after-init-hook '(lambda ()
							  ;(server-start)
							  (split-window-horizontally)
							  (list-bookmarks)
							  (switch-to-buffer (get-buffer-create "*Bookmark List*"))))

(setq kill-buffer-query-functions (remove 'process-kill-buffer-query-function kill-buffer-query-functions))

(defadvice ibuffer-do-print (before print-buffer-query activate)
   (unless (y-or-n-p "Print buffer? ")
     (error "Cancelled")))

;(require 'dired+)
;(put 'dired-find-alternate-file 'disabled nil)  ;enable `a' command

;;这个东西必须放在最后%%
;;desktop.el是一个可以保存你上次emacs关闭时的状态，下一次启动时恢复为上次关闭的状态。就和vmware的suspend一样。
;;因为我要使用sawfish-mode,wiki-mode,html-helper-mode，放在这里才能保证下次启动时能正确辨认文件需要的模式。
;(load "desktop") 
;(desktop-load-default) 
;(desktop-read)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((Syntax . Common-Lisp) (eval add-hook (quote write-file-hooks) (quote time-stamp)))))
 '(scheme-program-name "guile -q -l /media/D/qachina/db/doc/scheme/money-guile.scm"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(erc-command-indicator-face ((t (:foreground "red"))))
 '(vertical-border ((nil (:background "black" :foreground "black")))))
