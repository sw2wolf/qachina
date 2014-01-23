;; (add-to-list 'load-path "~/.emacs.d/scheme")

;; (autoload 'scheme-mode "iuscheme" "Major mode for Scheme." t)
;; (autoload 'run-scheme "iuscheme" "Switch to interactive Scheme buffer." t)

(setq auto-mode-alist (cons '("\\.ss" . scheme-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.sls" . scheme-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.sld" . scheme-mode) auto-mode-alist))

;; (autoload 'balanced-toggle "balanced" "Toggle balanced ``mode''" t)
;; (autoload 'balanced-on "balanced" "Turn on balanced ``mode''" t)
;; (add-hook 'scheme-mode-hook 'balanced-on)

;(custom-set-variables '(scheme-program-name "huski"))
;(custom-set-variables '(scheme-program-name "gsi"))
;(custom-set-variables '(scheme-program-name "csi -q -n"))
(custom-set-variables '(scheme-program-name "guile -q -l /media/D/qachina/db/doc/scheme/money-guile.scm"))

;
; geiser
;
;; (load "~/test/geiser/build/elisp/geiser-load")

;; (setq geiser-repl-startup-time 100)
;; (setq geiser-repl-query-on-kill-p nil)
;; (setq geiser-repl-history-filename "~/.emacs.d/geiser-history")

;; (setq geiser-active-implementations '(guile))
;; (setq geiser-guile-binary "/usr/local/bin/guile")
;; ;(setq geiser-guile-load-init-file-p nil)
;; (setq geiser-guile-init-file "~/.guile")
