;;设置有用的个人信息,这在很多地方有用。
(setq user-full-name "sw2wolf")
(setq user-mail-address "***@163.com")

;;去掉Emacs和gnus启动时的引导界面
(setq inhibit-startup-message t)
(setq gnus-inhibit-startup-message t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode nil)
(transient-mark-mode t)
(set-scroll-bar-mode nil)
(cua-selection-mode 1)

(delete-selection-mode +1)

(set-language-environment "UTF-8")
(set-background-color "Black") 
(set-foreground-color "White")

(add-to-list 'load-path "~/.emacs.d/")
;(setq default-directory "/media/E/www/qachina/db/doc/money")
(setq bookmark-default-file "~/.emacs.d/.emacs.bmk")
;; 同步更新书签文件 ;; 或者退出时保存
(setq bookmark-save-flag 1)

;(setenv "JAVA_HOME" "/usr/lib/jvm/jdk1.6.0_35")
;(setenv "PATH" (concat (getenv "PATH") ":" (getenv "JAVA_HOME") "/bin"))

;; 一打开就起用 text 模式。  
(setq default-major-mode 'text-mode)

;; 以 y/n代表 yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;;粘贴于光标处，而不是鼠标指针处
(setq mouse-yank-at-point t)

;; 显示时间，格式如下
(display-time-mode 1) 
(setq display-time-24hr-format t) 
(setq display-time-day-and-date t)

(set-default-font "Bitstream Vera Sans Mono-10")
;; 设置中文字体
(set-fontset-font "fontset-default"
      'gb18030 '("WenQuanYi Bitmap Song" . "unicode-bmp"))
;; 设置 sentence-end 可以识别中文标点
(setq sentence-end
      "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

(setq line-number-mode t)
;(global-linum-mode 'linum-mode);;在左边显示行号
(setq column-number-mode t) 

;自定义缩进长度
(setq standard-indent 4)

;; 显示括号匹配
(show-paren-mode t)
(setq show-paren-style 'parentheses)

;; 语法高亮
(global-font-lock-mode t)

;; 显示时间，格式如下
(display-time-mode 1)  
(setq display-time-24hr-format t)  
(setq display-time-day-and-date t)  

;; 支持emacs和外部程序的粘贴
(setq x-select-enable-clipboard t)

;; 在标题栏提示你目前在什么位置
(setq frame-title-format "emacs@%b")

;; 默认显示 80列就换行
(setq default-fill-column 80)

;; 设置默认tab宽度
(setq tab-width 4
      indent-tabs-mode t
      c-basic-offset 4)
(setq default-tab-width 4)

;;不要临时文件
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq backup-inhibited t);;不产生备份

;;滚动页面时比较舒服，不要整页的滚动
(setq scroll-step 1
      scroll-margin 3
      scroll-conservatively 10000)

;;在文档最后自动插入空白一行，好像某些系统配置文件是需要这样的
(setq require-final-newline t)
;; 当光标在行尾上下移动的时候，始终保持在行尾。 
(setq track-eol t)

;;使用C-k删掉指针到改行末的所有东西
(setq-default kill-whole-line t)

;;没有提示音，也不闪屏
(setq ring-bell-function 'ignore)

;;去掉警告铃声
(setq visible-bell nil)

;;光标显示为一竖线
(setq-default cursor-type 'bar)

;;可以显示图片
(auto-image-file-mode t)

;;光标靠近鼠标指针时，让鼠标指针自动让开，别挡住视线。
(setq mouse-avoidance-mode 'animate)

;; 填入大中小括号，双单引号的匹配
;; 详细格式可以参照C-h f skeleton-pair-alist  
;; (setq skeleton-pair-alist  
;;       '((?\" _ "\"" >)
;;         ;(?\' _ "\'" >)
;;         (?\( _ ")" >)
;;         (?\[ _ "]" >)
;;         (?\{ _ "}" >)))

(setq skeleton-pair t)

(mapcar
 (function (lambda (setting)
	     (setq auto-mode-alist
		   (cons setting auto-mode-alist))))
 '(("\\.xml$" .  sgml-mode)
   ("\\.org\\'" . org-mode)
   ("\\\.bash" . sh-mode)
   ("\\.rdf$" .  sgml-mode)
   ("\\.session" . emacs-lisp-mode)
   ("\\.l$" . c-mode)
   ("\\.hs$" . haskell-mode)
   ("\\.ml[iyl]?$" . caml-mode)
   ;("\\.pl$" . prolog-mode)
   ("\\.fs\\'" . forth-mode)
   ("\\.css$" . css-mode)
   ("\\.cfm$" . html-mode)
   ("gnus" . emacs-lisp-mode)
   ("\\.clj$" . clojure-mode)
   ("\\.rkt$" . geiser-mode)
   ("\\.py$" . python-mode)
   ("\\.idl$" . idl-mode)))

;;默认链接网络浏览器打开
(setq browse-url-generic-program (executable-find "opera")
       browse-url-browser-function 'browse-url-generic)

(require 'grep)
(grep-apply-setting 'grep-command "grep -nH -R -e  ")
