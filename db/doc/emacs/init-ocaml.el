(add-to-list 'load-path "~/.emacs.d/ocaml-mode")
(add-to-list 'auto-mode-alist '("\\.ml[iylp]?$" . caml-mode))
(autoload 'caml-mode "caml" "Major mode for editing OCaml code." t)
(autoload 'run-caml "inf-caml" "Run an inferior OCaml process." t)
(autoload 'camldebug "camldebug" "Run ocamldebug on program." t)
(add-to-list 'interpreter-mode-alist '("ocamlrun" . caml-mode))
(add-to-list 'interpreter-mode-alist '("ocaml" . caml-mode))

(if window-system (require 'caml-font))

;; tuareg-mode
;; (add-to-list 'load-path "~/.emacs.d/tuareg-mode-1.45.7")
;; (setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
;; (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
;; (autoload 'camldebug "camldebug" "Run the Caml debugger" t)

;; flymake-ocaml
;; (require 'flymake)

;; (defun flymake-ocaml-init ()
;;   (flymake-simple-make-init-impl
;;    'flymake-create-temp-with-folder-structure nil nil
;;    (file-name-nondirectory buffer-file-name)
;;    'flymake-get-ocaml-cmdline))

;; (defun flymake-get-ocaml-cmdline (source base-dir)
;;   (list "ocaml_flycheck.pl"

;; ;; 这里flymake需要调用一个名为"ocaml_flycheck.pl"的perl脚本。
;; ;; 自己调整perl脚本的位置, 默认与源代码同目录。

;; (list source base-dir)))

;; (push '(".+\\.ml[yilp]?$" flymake-ocaml-init flymake-simple-java-cleanup)
;;       flymake-allowed-file-name-masks)
;; (push
;;  '("^\\(\.+\.ml[yilp]?\\|\.lhs\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)"
;;    1 2 3 4) flymake-err-line-patterns)

;; ;; 如果你要flymake自动执行的话……
;; (add-hook
;;  'tuareg-mode-hook
;;  '(lambda ()
;;     (if (not (null buffer-file-name)) (flymake-mode))))
