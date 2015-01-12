(add-to-list 'load-path "~/.emacs.d/elpa/lua-mode-20110428")

(require 'lua-mode)
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist)) 
(autoload 'lua-mode "lua-mode" "Lua editing mode." t) 
(add-hook 'lua-mode-hook 'turn-on-font-lock) 
