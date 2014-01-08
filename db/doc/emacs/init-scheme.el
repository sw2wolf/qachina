(load "~/test/geiser/build/elisp/geiser-load")
;(load-file "~/.emacs.d/geiser/elisp/geiser.el")

(setq geiser-repl-startup-time 100)
(setq geiser-repl-query-on-kill-p nil)
(setq geiser-repl-history-filename "~/.emacs.d/geiser-history")

(setq geiser-active-implementations '(guile))
(setq geiser-guile-binary "/usr/local/bin/guile")
;(setq geiser-guile-load-path ...)
(setq geiser-guile-load-init-file-p nil)
(setq geiser-guile-init-file "~/.guile")
