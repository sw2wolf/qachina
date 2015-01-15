(require 'eshell)

(setenv "EDITOR" "emacsclient")
(setenv "LANG" "UTF-8")

(defun eshell/clear ()
  "clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun eshell/eval ()
  (interactive) 
  ;(insert "clisp.sh '(m:)'")
  ;(insert "sb.sh '(princ ())'")
  (insert "ccl.sh \"(princ )\"")
  (backward-char 3) ;(goto-char (- (point) 2))
)

(defun eshell/other ()
  (interactive)
  ;(insert "max.sh ':lisp (m:)'")
  ;(insert "pl.sh ''")
  ;(insert "max.sh ''") 
  ;(insert "ml ''")
  (insert "ccl.sh '(m:)'")
  (backward-char 2)
)

;; (setq eshell-prompt-function
;;       (lambda ()
;; 	(concat "" (user-login-name) "@" (system-name) " "
;; 		(eshell/pwd) "% ")))

(setq eshell-banner-message "")

(setq eshell-mode-hook nil)
(add-hook 'eshell-mode-hook (lambda () 
    ;(outline-minor-mode 1)
    ;; (color-theme-my-eshell)
    ;; (setq outline-regexp "^[^#$\n]* [#>]+ "
    ;;       scroll-margin 0
    ;;       eshell-scroll-to-bottom-on-output t
    ;;       eshell-scroll-show-maximum-output t)

    (add-to-list 'eshell-output-filter-functions 
                 'eshell-postoutput-scroll-to-bottom)

  ;(define-key eshell-mode-map [(control p)] 'eshell-previous-matching-input-from-input)
  ;(define-key eshell-mode-map [(control n)] 'eshell-next-matching-input-from-input)
 
  (define-key eshell-mode-map [up] 'previous-line)
  (define-key eshell-mode-map [down] 'next-line)
 
  ;(define-key eshell-mode-map [home] 'eshell-bol)

  ;(define-key eshell-mode-map [(control u)] 'eshell-kill-input) ;删除已输入命令
  ;(define-key eshell-mode-map [(control l)] 'eshell/clear)
  ;(define-key eshell-mode-map [(control e)] 'eshell/eval)
  ;(define-key eshell-mode-map [(control b)] 'eshell/erlang)

  ;(local-set-key (kbd "C-p") 'eshell-previous-matching-input-from-input)
  ;(local-set-key (kbd "C-n") 'eshell-next-matching-input-from-input)
  ;(local-set-key (kbd "<up>") 'previous-line)
  ;(local-set-key (kbd "<down>") 'next-line)

  (local-set-key (kbd "C-l") 'eshell/clear)
  (local-set-key (kbd "C-e") 'eshell/eval)
  (local-set-key (kbd "C-c e") 'eshell/other)
  ;(local-set-key (kbd "C-c e") 'eshell/erlang)
))

(add-hook 'comint-output-filter-functions 'comint-watch-for-password-promt)

(defalias 'img
  (lambda(img)
	(propertize "Image" (quote display)
				(create-image (expand-file-name img)))))

(defalias 'ff 'find-file)
;(defalias 'ee (lambda()(find-file (expand-file-name "44eshell.el" init-dir))))
;(defalias 'aa (lambda()(find-file eshell-aliases-file)))
;(defalias 'rr (lambda()(find-file (expand-file-name "qref.org" sand-box))))
(defalias 'ss  'shell-command-to-string)

;; (require 'em-alias)
;; (add-to-list 'eshell-command-aliases-list (list "ls" "ls -l"))

;
; misc.
;
;alias ff 'find-file $1'
;alias d 'dired $1'
;; listify ARGS	Parses an argument string into elisp list notation and prints it to the screen. It’s clever enough to handle both MS-DOS/Windows and POSIX-style argument syntax.
;; addpath PATH	Adds the argument, which must be a path, to the $PATH environment variable. If no argument is specified the existing paths are pretty-printed to the screen.
;; unset ENV-VAR	Unsets an existing environment variable
;; find-file FILE	Finds the file FILE and opens it in Emacs. This function is TRAMP aware and will therefore work remotely.
;; dired DIRECTORY	Opens a dired buffer in DIRECTORY.
;; calc-eval EXPR	Runs EXPR through the Emacs calculator.
;; upcase STR/downcase STR	Converts STR to upper- or lowercase.
;; vc-dir DIRECTORY	Reports the status of a version controlled directory (equivalent to the status command in most VCS)
;; ediff-files FILE1 FILE2	Diffs FILE1 and FILE2 using ediff, Emacs’ diff engine.

;; echo "hello world" >> #<buffer eshell.txt>
;; echo hello > /dev/clip    
;; echo hello > /dev/kill

;; setq foobar ${date}

;; C-c M-b will insert the printed name of a buffer
