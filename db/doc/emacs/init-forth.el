(load "~/.emacs.d/gforth")

(add-hook 'forth-mode-hook (function (lambda ()
        ;; customize variables here:
        (setq forth-indent-level 4)
        (setq forth-minor-indent-level 2)
        (setq forth-hilight-level 3)
        ;;; ...
)))
