;; 让 shell mode 可以正常显示颜色
;(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
;(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun eshell/clear ()
  "clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;; (setq eshell-prompt-function
;;       (lambda ()
;; 	(concat "" (user-login-name) "@" (system-name) " "
;; 		(eshell/pwd) "% ")))

;; echo "hello world" >> #<buffer eshell.txt>
;; echo hello > /dev/clip    
;; echo hello > /dev/kill

