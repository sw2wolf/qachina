(require 'package)

(setq package-archives
      '(("gnu"         . "http://elpa.gnu.org/packages/")
        ("original"    . "http://tromey.com/elpa/")
        ("org"         . "http://orgmode.org/elpa/")
        ("marmalade"   . "http://marmalade-repo.org/packages/")
        ("melpa"       . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; (setq core-packages
;;       '(;;list of packages you want
;;     magit
;;     nyan-mode
;;     projectile
;;     rainbow-delimiters
;;     undo-tree
;;         ace-jump-mode
;;     ace-window
;;     smartparens))

(unless package-archive-contents
  (package-refresh-contents))

;; (defun ensure-packages (packages)
;;   (dolist (package packages)
;;     (unless (package-installed-p package)
;;       (package-install package))))

;; (ensure-packages core-packages)
