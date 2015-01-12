;; (add-to-list 'load-path "~/.emacs.d/scheme")

;; (autoload 'scheme-mode "iuscheme" "Major mode for Scheme." t)
;; (autoload 'run-scheme "iuscheme" "Switch to interactive Scheme buffer." t)

(add-to-list 'auto-mode-alist '("\\.ss$" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.sls$" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.sld$" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.scm$" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.rkt$" . scheme-mode))

;; (autoload 'balanced-toggle "balanced" "Toggle balanced ``mode''" t)
;; (autoload 'balanced-on "balanced" "Turn on balanced ``mode''" t)
;; (add-hook 'scheme-mode-hook 'balanced-on)

;(custom-set-variables '(scheme-program-name "huski"))
;(custom-set-variables '(scheme-program-name "gsi"))
;(custom-set-variables '(scheme-program-name "csi -q -n"))
(custom-set-variables '(scheme-program-name "guile -q -l /media/D/qachina/db/doc/scheme/money-guile.scm"))

; gambit
;; (autoload 'gambit-inferior-mode "gambit" "Hook Gambit mode into cmuscheme.")
;; (autoload 'gambit-mode "gambit" "Hook Gambit mode into scheme.")
;; (add-hook 'inferior-scheme-mode-hook (function gambit-inferior-mode))
;; (add-hook 'scheme-mode-hook (function gambit-mode))
;; (setq scheme-program-name "gsc -:d-")

;
; geiser
;
;; (load "~/test/geiser/build/elisp/geiser-load")
;; (add-to-list 'auto-mode-alist '("\\.rkt$" . geiser-mode))
;; (setq geiser-repl-startup-time 100)
;; (setq geiser-repl-query-on-kill-p nil)
;; (setq geiser-repl-history-filename "~/.emacs.d/geiser-history")

;; (setq geiser-active-implementations '(guile))
;; (setq geiser-guile-binary "/usr/local/bin/guile")
;; ;(setq geiser-guile-load-init-file-p nil)
;; (setq geiser-guile-init-file "~/.guile")
