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

(defun qachina ()
   (interactive)
   (async-shell-command "cd /media/D/qachina && ./start.bat" "*QA-China*"))

;; (defun erlang ()
;;   (interactive)
;;   (async-shell-command "~/bin/yw" "*erlang*"))

;; (defun ocaml ()
;;   (interactive)
;;   (run-caml "~/bin/ml"))

;; (defun jump-run-guile ()
;;   (interactive)
;;   (if (get-buffer "*guile*")
;; 	  (switch-to-buffer-other-window "*guile*")
;; 	  (async-shell-command "~/bin/g" "*guile*")))

;; (defun jump-run-prolog ()
;;   (interactive)
;;   (if (get-buffer "*swi-prolog*")
;; 	  (switch-to-buffer-other-window "*swi-prolog*")
;; 	  (async-shell-command "~/bin/pl" "*swi-prolog*")))

;; (defun jump-run-haskell ()
;;   (interactive)
;;   (if (get-buffer "*ghci*")
;; 	  (switch-to-buffer-other-window "*ghci*")
;; 	  (async-shell-command "~/bin/hs" "*ghci*")))
(defun clisp ()
  (interactive)
  (if (get-buffer "*clisp*")
	  (switch-to-buffer-other-window "*clisp*")
	  (async-shell-command "clisp -q -modern -ansi" "*clisp*")))

(defun jump-run-clisp ()
  (interactive)
  (let ((repl (find-if (lambda (buff)
			 (string-match "^*slime-repl" (buffer-name buff))) (buffer-list))))
  (if repl
	  (switch-to-buffer-other-window (buffer-name repl))
	  (condition-case e (slime-connect "127.0.0.1" 4005)
		(file-error (clisp))))))

(defun jump-run-mew ()
  (interactive)
  (if (get-buffer "+inbox")
	  (switch-to-buffer-other-window "+inbox")
	  (mew)))

(defun jump-run-shell ()
  (interactive)
  (if (get-buffer "*ansi-term*")
	  (switch-to-buffer "*ansi-term*")
	  (ansi-term "csh"))
  ;(let ((proc (start-process "cmd" nil "cmd.exe" "/C" "start" "cmd.exe")))
  ;(set-process-query-on-exit-flag proc nil))
)

;; (remove-if-not (lambda (buff)
;; 				 (string-match "\\.p[lm]$" (buffer-name buff))) (buffer-list))
;; (some (lambda (buff) (string-match "^*slime-repl" (buffer-name buff))) (buffer-list))

(require 'thingatpt)
(global-set-key (kbd "<f1>") 'forward-whitespace)
(global-set-key (kbd "<f2>") 'find-file-at-point)
(global-set-key (kbd "<f3>") 'describe-char)

(global-set-key (kbd "<f4>") 'jump-run-shell)

;(global-set-key (kbd "<f5>") '(lambda () (interactive) (insert #x3bb)))
;√:#x221a π:#x3c0 λ:#x3bb ∑:#x2211 ⊥:#x22a5 ≅:#x2245 ≠:#x2260 ☺:#x263a
(global-set-key (kbd "<f5>") 'jump-run-clisp)
(global-set-key (kbd "<f6>") 'jump-run-mew)
(global-set-key (kbd "<f7>") '(lambda () (interactive) (w3m)))

(global-set-key (kbd "<f8>") '(lambda () (interactive) (insert "/msg lambdabot @ty ")))
; > @wn @src @where @undo @unmtl @pl @package
;@djinn turn a type into its corresponding expression

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
