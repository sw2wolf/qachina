(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/clojure-mode")
(add-to-list 'load-path "~/.emacs.d/ocaml-mode")
(add-to-list 'load-path "/usr/local/lib/erlang/lib/tools-2.6.6.5/emacs/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ELPA;;;;;;;;;;;;;;
;; (when (equal emacs-major-version 24)
;;   (load "package")
;;   (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;;   (package-initialize)
;; )

(load "init-base")
(load "init-key")
(load "init-eshell")
(load "init-erc")
(load "init-dict")
;(load "init-haskell")
;(load "init-ruby")
;(load "init-clojure")
;(load "init-racket")
;(load "init-forth")
;(load "init-slime")
;(load "init-emms")
;(load "init-radio")

(require 'erlang-start)
(require 'ocaml)

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
(add-hook 'after-init-hook 'split-window-horizontally)

;(require 'dired+)
;(put 'dired-find-alternate-file 'disabled nil)  ;enable `a' command

;;这个东西必须放在最后%%
;;desktop.el是一个可以保存你上次emacs关闭时的状态，下一次启动时恢复为上次关闭的状态。就和vmware的suspend一样。
;;因为我要使用sawfish-mode,wiki-mode,html-helper-mode，放在这里才能保证下次启动时能正确辨认文件需要的模式。
;(load "desktop") 
;(desktop-load-default) 
;(desktop-read)
