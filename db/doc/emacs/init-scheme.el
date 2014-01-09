(add-to-list 'load-path "~/.emacs.d/scheme")

(autoload 'scheme-mode "iuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "iuscheme" "Switch to interactive Scheme buffer." t)
(setq auto-mode-alist (cons '("\\.ss" . scheme-mode) auto-mode-alist))

(autoload 'balanced-toggle "balanced" "Toggle balanced ``mode''" t)
(autoload 'balanced-on "balanced" "Turn on balanced ``mode''" t)
(add-hook 'scheme-mode-hook 'balanced-on)

(custom-set-variables '(scheme-program-name "petite"))
;(setq scheme-program-name "petite")

;
; Guile
;
;(load "~/test/geiser/build/elisp/geiser-load")
;(load-file "~/.emacs.d/geiser/elisp/geiser.el")

;(setq geiser-repl-startup-time 100)
;(setq geiser-repl-query-on-kill-p nil)
;(setq geiser-repl-history-filename "~/.emacs.d/geiser-history")

;(setq geiser-active-implementations '(guile))
;(setq geiser-guile-binary "/usr/local/bin/guile")
;(setq geiser-guile-load-path ...)
;(setq geiser-guile-load-init-file-p nil)
;(setq geiser-guile-init-file "~/.guile")
