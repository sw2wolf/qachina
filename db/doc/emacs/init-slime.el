(setq common-lisp-hyperspec-root "~/HyperSpec/")

(load (expand-file-name "~/quicklisp/slime-helper.el"))

(setq inferior-lisp-program "~/ccl/fx86cl -Q -K utf-8")
;(setq inferior-lisp-program "~/clisp/bin/clisp -q -q -modern -ansi")
;(setq inferior-lisp-program "/usr/local/bin/sbcl")

(setq slime-multiprocessing t)
(setq slime-net-coding-system 'utf-8-unix)

;用另一种方法来指定多个Lisp实现, 然后就可以使用M-- M-x slime命令(M键+减号、M键+x)然后选择我们定义的任意一个Lisp实现了，只需要指定上述定义的NAME即可。
;(setq slime-lisp-implementations
;    '((sbcl ("/usr/local/bin/sbcl") :coding-system utf-8-unix)
;      (cmucl ("/usr/local/bin/cmucl") :coding-system iso-latin-1-unix)))
;; (setq slime-lisp-implementations
;;      '((sbcl ("sbcl" "--core" "my-sbcl.core"))))

;; (setq browse-url-browser-function
;;     '(("/home/sw2wolf/HyperSpec" . w3m-browse-url)
;;       ("." . browse-url-default-browser)))

;(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/slime")
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

;; (defun slime-repl-setup-initial-packages ()
;;   (loop for package in '(:ab :project-a :project-b :project-c)
;;    do (when (slime-eval `(cl:if (cl:find-package ,package) t))
;;          (slime-repl-set-package package)
;;          (return))))

;; (defun slime-repl-setup-initial-packages ()
;;   (slime-repl-set-package :money))
;; (add-hook 'slime-connected-hook 'slime-repl-setup-initial-packages t)

;; To make SLIME connect to your lisp whenever you open a lisp file
;; (add-hook 'slime-mode-hook
;; 		  (lambda ()
;; 			(unless (slime-connected-p)
;; 			  (save-excursion (slime)))))
