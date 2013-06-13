;default-input-method
;C-x RET C-\ , then choose an input method.
;
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/clojure-mode")
(add-to-list 'load-path "~/.emacs.d/ocaml-mode")
(add-to-list 'load-path "/usr/local/lib/erlang/lib/tools-2.6.8/emacs/")
(add-to-list 'load-path "~/.emacs.d/git-emacs")
(add-to-list 'load-path "~/.emacs.d/mu4e")
(add-to-list 'load-path "~/RnD/w3m")

;(load "init-package")
(load "init-base")
(load "init-key")

(load "init-dict")

(load "init-erc")
;(load "init-w3m")
(load "init-mew")

(load "init-haskell")
(load "init-prolog")

(load "init-slime")

;(load "init-clojure")
;(load "init-racket")
;(load "init-ruby")
;(load "init-forth")

;(require 'erlang-start)
(require 'ocaml)
;(require 'git-emacs)

(load "init-eshell")
;(load "init-mu4e")

;(load "init-emms")
(load "init-radio")

(setq shell-file-name "/bin/csh")

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; ------------------------------------------------------------ [ ispell ]
(eval-after-load "ispell"
  '(progn
     ;; Use the -C option when running aspell, which will
     ;; ConsiderCamelCaseToBeCorrect
     (setq ispell-extra-args '("-C"))))

;(setq initial-frame-alist '((top . 0) (left . 0) (width . 1024) (height . 768)))
(add-hook 'after-init-hook '(lambda ()
							  ;(server-start)
							  (split-window-horizontally)
							  (list-bookmarks)
							  (switch-to-buffer (get-buffer-create "*Bookmark List*"))))

(setq kill-buffer-query-functions (remove 'process-kill-buffer-query-function kill-buffer-query-functions))

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
 )
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(vertical-border ((nil (:background "black" :foreground "black")))))
