(load "~/.emacs.d/prolog")

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
                               auto-mode-alist))

(defun prolog-return( ) (interactive) (prolog-indent-line) (newline-and-indent))
(add-hook 'prolog-mode-hook
   '(lambda() 
        (local-set-key [13] 'prolog-return)   ;;; RET with automatic indent
        (local-set-key "\ep" 'indent-all)     ;;; Esc-p pretty-prints file
    ))
