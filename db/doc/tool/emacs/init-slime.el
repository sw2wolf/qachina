(setq common-lisp-hyperspec-root "~/HyperSpec")

(load (expand-file-name "~/quicklisp/slime-helper.el"))

;(setq inferior-lisp-program "/usr/local/bin/clisp")
;(setq inferior-lisp-program "/usr/local/bin/sbcl")
;(setq inferior-lisp-program "D:/ccl/wx86cl -K utf-8")

(setq slime-multiprocessing t)
(setq slime-net-coding-system 'utf-8-unix)

;(setq slime-lisp-implementations
;    '((sbcl ("/usr/local/bin/sbcl") :coding-system utf-8-unix)
;      (cmucl ("/usr/local/bin/cmucl") :coding-system iso-latin-1-unix)))
;(setq common-lisp-hyperspec-root "media/E/RnD/clisp/HyperSpec")
;(setq browse-url-browser-function
;      '(("/media/E/RnD/clisp/HyperSpec" . w3m-browse-url)
;        ("." . browse-url-default-browser)))

;(add-to-list 'load-path "/usr/share/emacs24/site-lisp/slime")
;(require 'slime)
;(slime-setup)
;(slime-setup '(slime-fancy))
;(require 'slime-autoloads)

;增加lisp代码的自动完成功能
(defun lisp-indent-or-complete (&optional arg)
    (interactive "p")
    (if (or (looking-back "^\\s-*") (bolp))
        (call-interactively 'lisp-indent-line)
        (call-interactively 'slime-indent-and-complete-symbol)))

(eval-after-load "lisp-mode"
    '(progn
        (define-key lisp-mode-map (kbd "TAB") 'lisp-indent-or-complete)))

;;按回车键后下一行代码自动缩进
(add-hook 'lisp-mode-hook '(lambda ()
      (local-set-key (kbd "RET") 'newline-and-indent))) 

(put 'upcase-region 'disabled nil)

