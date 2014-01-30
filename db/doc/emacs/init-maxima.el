(add-to-list 'load-path "~/maxima/share/maxima/5.32.1/emacs/")
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
(setq imaxima-use-maxima-mode-flag t)
(add-to-list 'auto-mode-alist '("\\.ma[cx]" . maxima-mode))

;(setq imaxima-fnt-size "Large")
;(setq imaxima-print-tex-command "latex %s; dvipdf %s.dvi imax.pdf; open imax.pdf")
;/usr/local/share/texmf/tex/
