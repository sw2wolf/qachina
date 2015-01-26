(require 'package)

(setq package-archives '(
   ("melpa" . "http://melpa.milkbox.net/packages/")
   ("gnu" . "http://elpa.gnu.org/packages/")))
(when (not package-archive-contents) (package-refresh-contents))
(package-initialize)
