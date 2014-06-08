(add-to-list 'load-path (expand-file-name "~/w3m/share/emacs/site-lisp/w3m"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              w3m mode
;;;
(require 'w3m-load)

(autoload 'w3m "w3m" "interface for w3m on emacs" t) 

(setq w3m-icon-directory "~/RnD/w3m/icons")
(setq w3m-home-page "http://www.baidu.com")

(setq w3m-command-arguments '("-cookie" "-F"))          ;;使用cookies和框架

(setq w3m-pop-up-windows nil)

(setq w3m-coding-system 'utf-8
	  w3m-default-coding-system 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-input-coding-system 'utf-8
      w3m-output-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8
	  w3m-bookmark-file-coding-system 'utf-8)

(setq w3m-use-form t)
(setq w3m-tab-width 8)
(setq w3m-use-cookies t)
(setq w3m-use-toolbar t)
(setq w3m-use-mule-ucs t)
(setq w3m-fill-column 120)
(setq w3m-default-display-inline-images t)              ;;打开图片显示  
(setq w3m-default-toggle-inline-images t)

;;显示图标 
(setq w3m-show-graphic-icons-in-header-line t)
(setq w3m-show-graphic-icons-in-mode-line t)

;;当用 shift+RET 打开新链接时将不自动跳转到新的页面，等提示已经完全打开，才用 C-c C-n ，
;;C-c C-p 打开，这个好用
(setq w3m-view-this-url-new-session-in-background t)

;; (defun w3m-browse-url-other-window (url &optional newwin)
;;   (interactive
;;    (browse-url-interactive-arg "w3m URL: "))
;;   (let ((pop-up-frames nil))
;;     (switch-to-buffer-other-window (w3m-get-buffer-create "*w3m*"))
;;     (w3m-browse-url url)))

;; (defun w3m-browse-url-new-tab (url &optional newwin)
;;   (interactive
;;    (browse-url-interactive-arg "w3m URL: "))
;;   (let ((pop-up-frames nil))
;;     (w3m-browse-url url t)))

;; (setq browse-url-browser-function
;;       '(("hoogle" . w3m-browse-url-other-window-new-tab)
;;         ("ghc" . w3m-browse-url-other-window-new-tab)
;;         ("hackage" . w3m-browse-url-other-window-new-tab)
;;         ("pylookup" . w3m-browse-url-new-tab)
;;         ("." .  browse-url-default-browser)))

(defun w3m-browse-url-other-window-new-tab (url &optional newwin)
  (interactive
   (browse-url-interactive-arg "w3m URL: "))
  (let ((pop-up-frames nil))
    (switch-to-buffer-other-window (w3m-get-buffer-create "*w3m*"))
    (w3m-browse-url url t)))
(setq browse-url-browser-function 'w3m-browse-url-other-window-new-tab)

;(setq browse-url-browser-function 'w3m-browse-url)      ;;设置为默认浏览器
;(setq browse-url-browser-function 'w3m-goto-url-new-session)

(eval-after-load "mm-decode"
'(progn 
   (add-to-list 'mm-discouraged-alternatives "text/richtext")))

(setq mm-text-html-renderer 'w3m
      mm-inline-text-html-with-images t
	  mm-inline-text-html-with-w3m-keymap nil
      mm-w3m-safe-url-regexp nil)

;; (defun remove-w3m-output-garbages ()
;; "去掉w3m输出的垃圾."
;;     (interactive)
;;     (let ((buffer-read-only))
;;         (setf (point) (point-min))
;;         (while (re-search-forward "[\200-\240]" nil t)
;;             (replace-match " "))
;;         (set-buffer-multibyte t))
;;     (set-buffer-modified-p nil))
;; (add-hook 'w3m-fontify-after-hook 'remove-w3m-output-garbages)

(add-hook
 'w3m-fontify-after-hook
 (lambda nil
   (let ((inhibit-read-only t))
     (save-excursion
	   (goto-char (point-min))
	   (while (re-search-forward "[\200-\240]" nil t)
		 (replace-match " "))))))

;; (while (re-search-forward " +$" nil t)
;;   (delete-region (match-beginning 0) (match-end 0)))

(defun my-w3m-rename-buffer (url)
  "Renames the current buffer to be the current URL"
  (rename-buffer url t))
(add-hook 'w3m-display-hook 'my-w3m-rename-buffer)

(add-hook 'w3m-display-hook
          (lambda (url)
            (let ((buffer-read-only nil))
              (delete-trailing-whitespace))))

;; (define-key w3m-mode-map (kbd "<") 'w3m-previous-buffer)
;; (define-key w3m-mode-map (kbd ">") 'w3m-next-buffer)

(standard-display-ascii ?\222 [?'])
(standard-display-ascii ?\225 [?+])
(standard-display-ascii ?\227 [?-])
(standard-display-ascii ?\240 [? ])
