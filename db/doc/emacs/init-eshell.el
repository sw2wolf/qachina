(require 'eshell)

(setenv "EDITOR" "emacsclient")

(defun eshell/clear ()
  "clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun eshell/eval1 ()
  (interactive)
  ;(insert "ml ';;'")
  ;(insert "hs ''")
  ;(insert "erl.sh 'user_default:'")
  ;(insert "clisp.sh '(m:)'")
  (insert "pl.sh ''")
  (backward-char 1) ;(goto-char (- (point) 2))
)

;; (defun eshell/eval2 ()
;;   (interactive)
;;   (insert "erl.sh 'io:format(\"~p~n\", [])'")
;;   (backward-char 3)
;; )

;; (setq eshell-prompt-function
;;       (lambda ()
;; 	(concat "" (user-login-name) "@" (system-name) " "
;; 		(eshell/pwd) "% ")))

(setq eshell-mode-hook nil)
(add-hook 'eshell-mode-hook (lambda () 
    ;(outline-minor-mode 1)
    ;; (color-theme-my-eshell)
    ;; (setq outline-regexp "^[^#$\n]* [#>]+ "
    ;;       scroll-margin 0
    ;;       eshell-scroll-to-bottom-on-output t
    ;;       eshell-scroll-show-maximum-output t)

    ;; (add-to-list 'eshell-output-filter-functions 
    ;;              'eshell-postoutput-scroll-to-bottom)

  (define-key eshell-mode-map [(control p)] 'eshell-previous-matching-input-from-input)
  (define-key eshell-mode-map [(control n)] 'eshell-next-matching-input-from-input)
 
  (define-key eshell-mode-map [up] 'previous-line)
  (define-key eshell-mode-map [down] 'next-line)
 
  (define-key eshell-mode-map [home] 'eshell-bol)
  (define-key eshell-mode-map [(control u)] 'eshell-kill-input) ;删除已输入命令

  (define-key eshell-mode-map [(control l)] 'eshell/clear)
  (define-key eshell-mode-map [(control e)] 'eshell/eval1)
  ;(define-key eshell-mode-map [(control t)] 'eshell/eval2)
))

(defalias 'img (lambda(img)
				 (propertize "Image" (quote display) 
							 (create-image (expand-file-name img)))))

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
