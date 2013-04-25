;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              w3m mode
;;;
(add-to-list 'load-path (concat path-prefix "w3m"))
(setq w3m-icon-directory (concat path-prefix "w3m/icons"))
(require 'w3m)
(add-to-list 'load-path "~/emacs-w3m/")
(require 'w3m-load)
(require 'mime-w3m)
(autoload 'w3m "w3m" "interface for w3m on emacs" t) 

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

;C-c C-p 打开，这个好用                                        
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

;; Make this comment if you have no a proper proxy
;(setq w3m-command-arguments-alist
;    '( ;; Don't use any additional options to visit local web pages.
;        ("^http://\\([^/]*\\.\\)*your_internal_website_name\\(/\\|$\\)" "-no-proxy")
        ;; Use the proxy server to visit any foreign urls.
;        ("" "-o" "http_proxy=http://your_proxy:80/")))
(global-set-key [f4] 'w3m)
