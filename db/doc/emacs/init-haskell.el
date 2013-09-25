(load "~/.emacs.d/haskell-mode/haskell-site-file")

(setq haskell-program-name "~/ghc/bin/ghci")
;(visit-tags-table "/media/D/qachina/db/doc/haskell")

;(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

;;Note that the three indentation modules are mutually exclusive - add at most one.
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;
; The following is from ghc-mod(cabal install ghc-mod)
;
;; (add-to-list 'load-path "~/.cabal/share/ghc-mod-2.0.3")
;; (require 'ghc)

;; (add-to-list 'load-path "~/.emacs.d/")
;; (autoload 'ghc-init "ghc" nil t)
;; (add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;if you want to use flymake automatically:
;; (autoload 'ghc-init "ghc" nil t)
;; (add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))

;; If you want to specify GHC options from Emacs, set "ghc-ghc-options". The following is an example to specify the "-i" options to GHC.
;(setq ghc-ghc-options '("-idir1" "-idir2"))

;An example to specify HLint options:
;(setq ghc-hlint-options '("--ignore=Use camelCase"))
;; M-C-i
;; Completes a name of keyword, module, class, function, types, language extensions, GHC flags, etc.

;; M-/
;; Completes a name of local symbol.

;; M-t
;; Inserts template. In the beginning of a buffer, "module Foo where" is inserted. On the function without signature, inferred type is inserted. On a symbol "foo" without definition, "foo = undefined" is inserted. Original code is replaced with hlint's suggestion if possible.

;; M-C-d
;; Browses the local document with your browser. On a module import line, the document of the module is browsed. On a function or type, its document is browsed.

;; C-uM-C-d
;; Browses the Hackage document of the module with your browser.

;; M-C-m
;; Loads information of symbols for modules in the current buffer. If you add a new line to import a module, type this. The idle timer executes this command anyway.

;; C-xC-s
;; Saves the buffer if necessary and runs syntax check. Also, a timer executes syntax check automatically.

;; C-cC-c
;; Toggle GHC and Hlint for syntax check. GHC is used for initial time.

;; M-n
;; Goes to the next warning or error.

;; M-p
;; Goes to the previous warning or error.

;; M-?
;; Displays the warning/error message in the current line.

;; C-cC-i
;; Displays the info of this expression in another window.

;; C-cC-t
;; Displays the type of this expression in the minibuffer. Type C-cC-t multiple time to enlarge the expression.

;; C-cC-e
;; Displays the expanded Template Haskell.

;; C-cC-m
;; Insert "import Module" for this function. "hoogle" command is required and "hoogle data" should be done beforehand.

;; C-cC-j
;; In the beginning of the buffer, errors of other files are displayed. Typing C-cC-j on the errors jumps to the fist file of the error sources.

;; C-c<
;; Make the indentation of the region shallower.

;; C-c>
;; Make the indentation of the region deeper.
