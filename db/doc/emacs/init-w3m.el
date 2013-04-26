;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              w3m mode
;;;
(require 'w3m)
;(require 'w3m-load)
;(require 'mime-w3m)
(autoload 'w3m "w3m" "interface for w3m on emacs" t) 

(setq w3m-icon-directory "~/RnD/w3m/icons")

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
(setq w3m-home-page "http://www.baidu.com")

(setq browse-url-browser-function 'w3m-browse-url)      ;;设置为默认浏览器

(eval-after-load "mm-decode" 
'(progn 
   (add-to-list 'mm-discouraged-alternatives "text/richtext")))

(setq mm-text-html-renderer 'w3m
      mm-inline-text-html-with-images t
	  mm-inline-text-html-with-w3m-keymap nil
      mm-w3m-safe-url-regexp nil)
 
;;当用 shift+RET 打开新链接时将不自动跳转到新的页面，等提示已经完全打开，才用 C-c C-n ，
;;C-c C-p 打开，这个好用
(setq w3m-view-this-url-new-session-in-background t)

;;显示图标 
(setq w3m-show-graphic-icons-in-header-line t) 
(setq w3m-show-graphic-icons-in-mode-line t)

(setq w3m-command-arguments '("-cookie" "-F"))          ;;使用cookies和框架

(add-hook 'w3m-fontify-after-hook 'remove-w3m-output-garbages)
(defun remove-w3m-output-garbages ()
"去掉w3m输出的垃圾."
    (interactive)
    (let ((buffer-read-only))
        (setf (point) (point-min))
        (while (re-search-forward "[\200-\240]" nil t)
            (replace-match " "))
        (set-buffer-multibyte t))
    (set-buffer-modified-p nil))
