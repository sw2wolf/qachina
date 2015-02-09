(add-to-list 'load-path "/mnt/D/RnD/haskell-mode")
(require 'haskell-mode-autoloads)
(add-to-list 'Info-default-directory-list "/mnt/D/RnD/haskell-mode/")

(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
