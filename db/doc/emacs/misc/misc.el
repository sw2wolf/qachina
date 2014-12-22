;;;自动启动ecb
;; (setq ecb-auto-activate t)
;;;一键开关
(defun my-ecb-active-or-deactive ()
 (interactive)
 (if ecb-minor-mode
     (ecb-deactivate)
   (ecb-activate)))

(global-set-key (kbd "<C-f1>") 'my-ecb-active-or-deactive)

;;; speedbar
(autoload 'speedbar-frame-mode "speedbar" "Popup a speedbar frame" t)
(autoload 'speedbar-get-focus "speedbar" "Jump to speedbar frame" t)

;;主浮页和speedbar浮页之间的切换快捷键——不用设置，因为系统本身可以使用M-Tab来实现

;;设置speedbar默认出现在左侧
(add-hook 'speedbar-mode-hook
        (lambda ()
         (auto-raise-mode 1)
         (add-to-list 'speedbar-frame-parameters '(top . 0))
         (add-to-list 'speedbar-frame-parameters '(left . 0))
         ))

;显示所有文件
(setq speedbar-show-unknown-files t)

;;设置tags排列顺序为按照出现的先后次序排列
(setq speedbar-tag-hierarchy-method '(speedbar-prefix-group-tag-hierarchy))

;;;默认以org-mode模式启动
(pop-to-buffer (get-buffer-create (generate-new-buffer-name "*scratch-org*")))
(insert "Scratch buffer with org-mode.\n\n")
(org-mode)

;;;在启动emacs的时候，显示日程表
(add-hook 'after-init-hook 'org-agenda-list)

;;=========================================================================
;;cedet插件设置
;;See cedet/common/cedet.info for configuration details.
;;=========================================================================
(add-to-list 'load-path "~/.emacs.d/plugins/cedet-1.0/speedbar")
(add-to-list 'load-path "~/.emacs.d/plugins/cedet-1.0/eieio")
(add-to-list 'load-path "~/.emacs.d/plugins/cedet-1.0/semantic")
(load-file                                              ;;Load CEDET.,从cedet的INSTALL中复制过来的
 "~/.emacs.d/plugins/cedet-1.0/common/cedet.el")
(global-ede-mode 1)               ;;Enable EDE (Project Management) features
(semantic-load-enable-minimum-features)
(semantic-load-enable-code-helpers)
(semantic-load-enable-semantic-debugging-helpers)
(global-ede-mode t)
;;==============================================================
;;ecb置
;;==============================================================

(require 'ecb)                  ;;开启ecb用,M-x:ecb-activate
(require 'ecb-autoloads)                        
(setq ecb-auto-activate t)
(setq ecb-tip-of-the-day nil
      ecb-tree-indent 4
      ecb-windows-height 0.5
      ecb-windows-width 0.15
      ecb-auto-compatibility-check nil
      ecb-version-check nil)
;;      (ecb-change-layout "leftright-analyse"))            ;;自动启动ecb并且不显示每日提示

(require 'cc-mode)

(c-set-offset 'inline-open 0)
(c-set-offset 'friend '-)
(c-set-offset 'substatement-open 0)
;;(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
(define-key c-mode-map [return] 'newline-and-indent)
(global-set-key [M-left] 'windmove-left)      ;;为了ecb窗口的切换
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)
(global-set-key [f11] 'ecb-hide-ecb-windows)      ;;隐藏和显示ecb窗口
(global-set-key [S-f11] 'ecb-show-ecb-windows)
(global-set-key (kbd "M-g") 'goto-line)         ;;设置M-g为goto-line
;;(global-set-key (kbd "C-SPC") 'nil)         ;;取消control+space键设为mark
(global-set-key (kbd "M-<SPC>") 'set-mark-command)   ;;这样 我就不用按 C-@ 来 setmark 了, C-@ 很不好按。
(global-set-key [home] 'beginning-of-buffer)      ;;设置home键指向buffer开头，end键指向buffer结尾
(global-set-key [end] 'end-of-buffer)
(global-set-key (kbd "C-,") 'scroll-left)      ;;"C-,"设为屏幕左移命令
(global-set-key (kbd "C-.") 'scroll-right)      ;;"C-."设为屏幕右移命令
(global-set-key (kbd "C-z") 'set-mark-command)      ;;C-z 设置标记
(global-set-key (kbd "M-4") 'delete-window)      ;;关闭当前窗口,alt+4
;;(global-set-key (kbd "M-4") 'kill-this-buffer)
(global-set-key (kbd "M-1") 'delete-other-windows)   ;;关闭其他窗口,alt+1
(global-set-key (kbd "M-2") 'split-window-vertically)   ;;水平分割窗口,alt+2
(global-set-key (kbd "M-3") 'split-window-horizontally)   ;;垂直分割窗口,alt+3
(global-set-key (kbd "M-0") 'other-window)      ;;切换到其他窗口，alt+0
(global-set-key (kbd "M-5") 'display-buffer-name)   ;;显示缓冲区完整名称
;;==========================================================
;;加载cscope
;;==========================================================
(require 'xcscope)
;;==========================================================
;; This snippet enables lua-mode
;; This line is not necessary, if lua-mode.el is already on your load-path
;;==========================================================
(add-to-list 'load-path "~/.emacs.d/plugins/lua-mode/")
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-hook 'lua-mode-hook 'turn-on-font-lock)
(add-hook 'lua-mode-hook hs-minor-mode)
(defcustom ac-modes
  '(emacs-lisp-mode
    lisp-interaction-mode
    c-mode cc-mode c++-mode clojure-mode java-mode
    perl-mode cperl-mode python-mode ruby-mode
    ecmascript-mode javascript-mode js2-mode php-mode css-mode
    makefile-mode sh-mode fortran-mode f90-mode ada-mode
    xml-mode sgml-mode
    lua-mode muse-mode org-mode)
  "Major modes `auto-complete-mode' can run on."
  :type '(repeat symbol)
  :group 'auto-complete)
