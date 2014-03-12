(global-unset-key "\C-z")

;; 绑定全局键值
;; 也可以绑定单独到某个mode，比如cc-mode (define-key cc-mode-map (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "(")  'skeleton-pair-insert-maybe)
(global-set-key (kbd "{")  'skeleton-pair-insert-maybe)
(global-set-key (kbd "\'") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[")  'skeleton-pair-insert-maybe)

(global-set-key (kbd "C-j") 'goto-line)  
;;设置C-/为undo,M-/为set-mark 
(global-set-key (kbd "C-/") 'undo) 
(global-set-key (kbd "C-?") 'redo)

;; 扩大或者缩小窗口（上下）,Ctrl+{} 
(global-set-key (kbd "C-}") 'enlarge-window) 
(global-set-key (kbd "C-{") 'shrink-window)

(global-set-key (kbd "C-<left>")  'windmove-left)
(global-set-key (kbd "C-<right>") 'windmove-right)
(global-set-key (kbd "C-<up>")    'windmove-up)
(global-set-key (kbd "C-<down>")  'windmove-down)

(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
;(global-set-key "\C-x\C-m" 'execute-extended-command)

;; (defun edit-current-file-as-root ()
;;   "Edit the file that is associated with the current buffer as root"
;;   (interactive)
;;   (if (buffer-file-name)
;;       (progn
;;         (setq file (concat "/sudo:root@localhost:" (buffer-file-name)))
;;         (find-file file))
;;     (message "Current buffer does not have an associated file.")))

;; (defun jump-run-prolog ()
;;   (interactive)
;;   (if (get-buffer "*prolog*")
;; 	  (switch-to-buffer-other-window "*prolog*")
;; 	  (run-prolog 'swi)))

(defun jump-run-erlang ()
  (interactive)
  (if (get-buffer "*erlang*")
	  (switch-to-buffer "*erlang*")
	  (run-erlang)))

(defun jump-run-lisp ()
  (interactive)
  (let ((repl (find-if (lambda (buff)
			 (string-match "^*slime-repl" (buffer-name buff))) (buffer-list))))
  (if repl
	  (switch-to-buffer-other-window (buffer-name repl))
	  (condition-case e (slime-connect "127.0.0.1" 4005)
		(file-error (slime))))))

(defun jump-run-clisp ()
   (interactive)
   (if (get-buffer "*CLISP*")
	  (switch-to-buffer-other-window "*CLISP*")
	 (async-shell-command "~/bin/cl" "*CLISP*")))

;(file-error (call-interactively 'run-lisp))))))
;(unless (slime-connected-p) (save-excursion (slime)))

(defun jump-run-mew ()
  (interactive)
  (if (get-buffer "+inbox")
	  (switch-to-buffer-other-window "+inbox")
	  (mew)))

;; (defun jump-to-lambdabot ()
;;   (interactive)
;;   (if (get-buffer "lambdabot")
;; 	  (switch-to-buffer-other-window "lambdabot")))

(defun jump-run-shell ()
  (interactive)
  (if (get-buffer "*eshell*")
	  (switch-to-buffer "*eshell*")
	  (eshell))
  ;(let ((proc (start-process "cmd" nil "cmd.exe" "/C" "start" "cmd.exe")))
  ;(set-process-query-on-exit-flag proc nil))
)

(defun jump-run-chez ()
   (interactive)
   (if (get-buffer "*CHEZ*")
	  (switch-to-buffer "*CHEZ*")
	 (async-shell-command "petite /media/D/qachina/db/doc/scheme/money-petite.scm" "*CHEZ*")))

;; (defun jump-run-husk ()
;;    (interactive)
;;    (if (get-buffer "*HUSK*")
;; 	  (switch-to-buffer "*HUSK*")
;; 	 (async-shell-command "huski -i /media/D/qachina/db/doc/scheme/money-husk.scm" "*HUSK*")))

;; (remove-if-not (lambda (buff)
;; 				 (string-match "\\.p[lm]$" (buffer-name buff))) (buffer-list))
;; (some (lambda (buff) (string-match "^*slime-repl" (buffer-name buff))) (buffer-list))

;; emacs -batch -f batch-byte-compile ~/.emacs.d/**/*.el

;; (defun byte-compile-init-dir ()
;;   "Byte-compile all your dotfiles."
;;   (interactive)
;;   (byte-recompile-directory user-emacs-directory 0))

(require 'thingatpt)
(global-set-key (kbd "<f1>") 'forward-whitespace)
(global-set-key (kbd "<f2>") 'find-file-at-point)
;(global-set-key (kbd "<f3>") 'describe-char)

;(global-set-key (kbd "<f3>") '(lambda () (interactive) (insert #x3bb)))
;√:#x221a π:#x3c0 λ:#x3bb ∑:#x2211 ⊥:#x22a5 ≅:#x2245 ≠:#x2260 ☺:#x263a
;⋆:#x22c6 ≅:#x2245

; > @wn @src @where @undo @unmtl @pl @package
;@djinn turn a type into its corresponding expression
(global-set-key (kbd "<f3>") '(lambda () (interactive) (insert "/msg lambdabot @ty ")))
;; (global-set-key (kbd "<f3>") '(lambda () 
;; 								(interactive)
;; 								(insert "/msg rudybot ()")
;; 								(backward-char 1)))

(global-set-key (kbd "<f4>") 'jump-run-shell)
(global-set-key (kbd "<f5>") 'jump-run-mew)

;(global-set-key (kbd "<f5>") 'jump-to-lambdabot)
;(global-set-key (kbd "<f5>") '(lambda () (interactive) (run-haskell)))
;(global-set-key (kbd "<f5>") '(lambda () (interactive) (run-caml "ocaml")))

;(global-set-key (kbd "<f6>") 'jump-run-erlang)
;(global-set-key (kbd "<f6>") 'jump-run-prolog)

(global-set-key (kbd "<f6>") 'jump-run-lisp)
;(global-set-key (kbd "<f7>") 'imaxima)
(global-set-key (kbd "<f7>") 'jump-run-clisp)

;(global-set-key (kbd "<f8>") 'jump-run-chez)
;(global-set-key (kbd "<f8>") 'jump-run-husk)
(global-set-key (kbd "<f8>") 'run-scheme)

(global-set-key [(f9)] 'list-bookmarks)
(global-set-key [(f10)] 'bookmark-set)

;; (global-set-key [f11] 'my-maximized) 
;; (defun my-maximized () 
;;     (interactive) 
;;     (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
;;     (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))
;(global-set-key [(f12)] 'desktop-save)

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
