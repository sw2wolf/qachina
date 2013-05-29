;; echo "hello world" >> #<buffer eshell.txt>
;; echo hello > /dev/clip    
;; echo hello > /dev/kill

(defun eshell/clear ()
  "clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;; (setq eshell-prompt-function
;;       (lambda ()
;; 	(concat "" (user-login-name) "@" (system-name) " "
;; 		(eshell/pwd) "% ")))

(defun m-eshell-hook () 
; define control p, control n and the up/down arrow
  (define-key eshell-mode-map [(control p)] 'eshell-previous-matching-input-from-input)
  (define-key eshell-mode-map [(control n)] 'eshell-next-matching-input-from-input)
 
  (define-key eshell-mode-map [up] 'previous-line)
  (define-key eshell-mode-map [down] 'next-line)
 
  (define-key eshell-mode-map [home] 'eshell-bol)
  (define-key eshell-mode-map [(control u)] 'eshell-kill-input) ;删除已输入命令

  (define-key eshell-mode-map [(control l)] 'eshell/clear)
)

(add-hook 'eshell-mode-hook 'm-eshell-hook)
