;(add-to-list 'load-path "/usr/local/share/maxima/5.29.1/emacs")
(add-to-list 'load-path "~/maxima/share/maxima/emacs/")

(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(add-to-list 'auto-mode-alist '("\\.ma[cx]" . maxima-mode))

(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
(setq imaxima-use-maxima-mode-flag t)
(setq imaxima-fnt-size "Large")
;(setq imaxima-print-tex-command "latex %s; dvipdf %s.dvi imax.pdf; open imax.pdf")
