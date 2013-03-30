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

;(global-set-key "\C-x\C-m" 'execute-extended-command)

(defun qachina ()
  (interactive)
  (async-shell-command "cd /media/D/qachina && ./start.bat" "*QA-China*"))

(defun haskell ()
  (interactive)
  (run-caml "~/bin/hs"))

(defun clisp ()
  (interactive)
  (async-shell-command "clisp -q -modern -ansi" "*clisp*"))

(defun erlang ()
  (interactive)
  (async-shell-command "~/bin/yw" "*erlang*"))

(defun jump-open-prolog ()
  (interactive)
  (if (get-buffer "*swi-prolog*")
	  (switch-to-buffer-other-window "*swi-prolog*")
	  (async-shell-command "~/bin/pl" "*swi-prolog*")))

(defun ocaml ()
  (interactive)
  (run-caml "~/bin/ml"))

;; 按下C-x k立即关闭掉当前的buffer
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(defun jump-open-shell ()
  (interactive)
  (if (get-buffer "*ansi-term*")
	  (switch-to-buffer "*ansi-term*")
	  (ansi-term "csh"))
  ;(let ((proc (start-process "cmd" nil "cmd.exe" "/C" "start" "cmd.exe")))
  ;(set-process-query-on-exit-flag proc nil))
)

(defun find-buffer (buff)
  (find-if (lambda (buff)
			 (string-match "^*slime-repl" (buffer-name buff))) (buffer-list)))

(defun jump-open-slime-repl ()
  (interactive)
  (let ((repl (find-if (lambda (buff)
			 (string-match "^*slime-repl" (buffer-name buff))) (buffer-list))))
  (if repl
	  (switch-to-buffer-other-window (buffer-name repl))
	  (slime-connect "127.0.01" 4005))))

;; (remove-if-not (lambda (buff)
;; 				 (string-match "\\.p[lm]$" (buffer-name buff))) (buffer-list))
;; (some (lambda (buff) (string-match "^*slime-repl" (buffer-name buff))) (buffer-list))

(global-set-key (kbd "M-j")
    (lambda ()
	  (interactive)
      (join-line -1)))

(require 'thingatpt)
(global-set-key (kbd "<f1>") 'forward-whitespace)
(global-set-key (kbd "<f2>") 'jump-open-shell)
(global-set-key (kbd "<f3>") 'find-file-at-point)
(global-set-key (kbd "<f4>") 'describe-char)

;(global-set-key (kbd "<f5>") '(lambda () (interactive) (insert #x3bb)))
(global-set-key (kbd "<f5>") 'jump-open-slime-repl)
(global-set-key (kbd "<f6>") 'jump-open-prolog)

;(global-set-key (kbd "<f6>") '(lambda () (interactive) (insert "/msg lambdabot > ")))
(global-set-key (kbd "<f7>") '(lambda () (interactive) (insert "/msg lambdabot @type ")))
(global-set-key (kbd "<f8>") '(lambda () (interactive) (insert "/msg lambdabot @wn ")))
; > @src @where

(global-set-key [(f9)] 'list-bookmarks)
(global-set-key [(f10)] 'bookmark-set)
;(global-set-key [(f12)] 'desktop-save)

;; (global-set-key [f11] 'my-maximized) 
;; (defun my-maximized () 
;;     (interactive) 
;;     (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
;;     (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
