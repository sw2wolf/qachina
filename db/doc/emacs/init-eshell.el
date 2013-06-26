(require 'eshell)

(setenv "EDITOR" "emacsclient")

(defun eshell/clear ()
  "clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun eshell/csh ()
  (interactive)
  (ansi-term "csh"))

;; (setq eshell-prompt-function
;;       (lambda ()
;; 	(concat "" (user-login-name) "@" (system-name) " "
;; 		(eshell/pwd) "% ")))

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
  (define-key eshell-mode-map [(control s)] 'eshell/csh)
))

;; (add-hook 'eshell-mode-hook (lambda()
;;     (def-key-s eshell-mode-map 
;;         "<up>"     'eshell-previous-matching-input-from-input
;;         "<down>"   'eshell-next-matching-input-from-input
;;         "<tab>"    'user-tab
;;         "<return>" 'user-ret
;;         "SPC"      'user-spc)
;; ))

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
