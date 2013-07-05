(add-to-list 'load-path "/usr/local/lib/erlang/lib/tools-2.6.8/emacs/")

;(setq erlang-root-dir "/usr/local/lib/erlang")
;(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))

(require 'erlang-start)

;(require 'erlang-flymake)

;;仅在存盘时进行检查
;(erlang-flymake-only-on-save)

;;键盘映射
;; (defvar flymake-mode-map (make-sparse-keymap))
;; (define-key flymake-mode-map (kbd "<f3>") 'flymake-goto-next-error)
;; (define-key flymake-mode-map (kbd "C-c <f3>") 'flymake-goto-prev-error)
;; (define-key flymake-mode-map (kbd "<f4>") 'flymake-display-err-menu-for-current-line)

;; (or (assoc 'flymake-mode minor-mode-map-alist)
;;     (setq minor-mode-map-alist
;;           (cons (cons 'flymake-mode flymake-mode-map)
;;                 minor-mode-map-alist)))
