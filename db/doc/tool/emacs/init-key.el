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

(defun clisp ()
  (interactive)
  (async-shell-command "clisp -q -modern -ansi" "*clisp*"))
(global-set-key "\C-c\C-l" 'clisp)

;; 按下C-x k立即关闭掉当前的buffer
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(defun open-shell-other-buffer ()
  "Open eshell in other buffer"
  (interactive)
  ;(split-window-horizontally)
  ;(split-window-vertically)
  ;(eshell)
  (ansi-term "csh")
  ;(let ((proc (start-process "cmd" nil "cmd.exe" "/C" "start" "cmd.exe")))
  ;(set-process-query-on-exit-flag proc nil))
)

(require 'thingatpt)
(global-set-key (kbd "<f1>") 'forward-whitespace)
(global-set-key (kbd "<f2>") 'open-shell-other-buffer)
(global-set-key (kbd "<f3>") 'find-file-at-point)
(global-set-key (kbd "<f4>") 'describe-char)
;(global-set-key (kbd "<f5>") '(lambda () (interactive) (insert #x3bb)))
(global-set-key (kbd "<f5>") '(lambda () (interactive) (insert "/msg rudybot ")))
(global-set-key (kbd "<f6>") '(lambda () (interactive) (insert "/msg lambdabot > ")))
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
