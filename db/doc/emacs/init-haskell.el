(load "~/.emacs.d/haskell-mode/haskell-site-file")

;(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

;;Note that the three indentation modules are mutually exclusive - add at most one.
;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(setq haskell-program-name "/home/sw2wolf/ghc/bin/ghci")
