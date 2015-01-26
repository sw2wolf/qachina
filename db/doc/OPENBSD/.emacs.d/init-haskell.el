(add-to-list 'load-path "~/.emacs.d/elpa/haskell-mode-13.7")
(require 'haskell-mode-autoloads)
(add-to-list 'Info-default-directory-list "~/.emacs.d/elpa/haskell-mode-13.7/")

(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
