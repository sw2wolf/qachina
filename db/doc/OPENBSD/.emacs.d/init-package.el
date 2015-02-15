;; (require 'package)

;; (setq package-archives '(
;;    ("melpa" . "http://melpa.milkbox.net/packages/")
;;    ("gnu" . "http://elpa.gnu.org/packages/")))
;; (when (not package-archive-contents) (package-refresh-contents))
;; (package-initialize)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
