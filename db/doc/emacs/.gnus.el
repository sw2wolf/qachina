(setq gnus-select-method '(nntp "news.newsfan.net"))  ;news.gmane.org
; Personal Information
(setq user-full-name "sw2wolf"
      user-mail-address "z9axis@gmail.com"
      ;message-generate-headers-first t
      )

;;;;;;;;;;;;;;;;;;;;  
;;   语言环境设定  
;;;;;;;;;;;;;;;;;;;;  
;; (set-language-environment 'Chinese-GB)
;; (setq gnus-default-charset 'chinese-iso-8bit
;;       gnus-group-name-charset-group-alist '((".*" . cn-gb-2312))
;;       gnus-summary-show-article-charset-alist
;;       '((1 . cn-gb-2312)
;;     (2 . gb18030)
;;     (3 . chinese-iso-8bit)
;;     (4 . gbk)
;;     (5 . big5)
;;     (6 . utf-8))
;;       gnus-newsgroup-ignored-charsets  
;;       '(unknown-8bit x-unknown iso-8859-1))

;;;;;;;;;;;;;;;;;;;;  
;;自动显示图片  
;;;;;;;;;;;;;;;;;;;;  
(auto-image-file-mode)
(setq mm-inline-large-images t)  
(add-to-list 'mm-attachment-override-types "image/*")

;; Change email address for work folder.  This is one of the most
;; interesting features of Gnus.  I plan on adding custom .sigs soon
;; for different mailing lists.
;; Usage, FROM: My Name <work>
(setq gnus-posting-styles
      '((".*"
     (name "sw2wolf"
          (address "***@gmail.com"
                   (organization "zsoft")
                   ;(signature-file "~/.signature")
				   (signature "e^pi+1=0")
                   ("X-Troll" "Emacs is better than Vi")
                   )))))

;; set email reader 
;nnfolder，nnmbox, nnml
;;@see http://gnus.org/manual/gnus_397.html
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "gmail"
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)
                      (nnir-search-engine imap)
                      (nnimap-authinfo-file "~/.authinfo.gpg")
                      ; @see http://www.gnu.org/software/emacs/manual/html_node/gnus/Expiring-Mail.html
                      ;; press 'E' to expire email
                      (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
                      (nnmail-expiry-wait 90)
                      )
             )

;; set pop server 
(setq mail-sources 
	  '((pop :server "pop.163.com"   ;; 在这里设置 pop3 服务器
             :user "sw2wolf"          ;; 用户名
             :port "pop3"
             :password "***")))      ;; 密码

;; set smtp 
(setq smtpmail-auth-credentials 
    '(("smtp.163.com"                ;; SMTP 服务器
       25                            ;; 端口号
       "sw2wolf"                      ;; 用户名
       "***")))                      ;; 密码
;(setq smtpmail-auth-credentials "~/.authinfo.gpg")

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "username@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-local-domain "homepc")

;;html转换成txt查看
(eval-after-load "mm-decode"
  '(progn
	 (add-to-list 'mm-discouraged-alternatives "text/html")
	 (add-to-list 'mm-discouraged-alternatives "text/richtext")))

;;;
;;; another configuration
;;;
; -*- Lisp -*-
(require 'nnir)

;;@see http://www.emacswiki.org/emacs/GnusGmail#toc1
(setq gnus-select-method '(nntp "news.gmane.org"))

;; ask encyption password once
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

(setq smtpmail-auth-credentials "~/.authinfo.gpg")

;;@see http://gnus.org/manual/gnus_397.html
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "gmail"
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)
                      (nnir-search-engine imap)
                      (nnimap-authinfo-file "~/.authinfo.gpg")
                      ; @see http://www.gnu.org/software/emacs/manual/html_node/gnus/Expiring-Mail.html
                      ;; press 'E' to expire email
                      (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
                      (nnmail-expiry-wait 90)
                      )
             )

(setq-default
  gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B%s%)\n"
  gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
  gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
  gnus-sum-thread-tree-false-root ""
  gnus-sum-thread-tree-indent ""
  gnus-sum-thread-tree-leaf-with-other "-> "
  gnus-sum-thread-tree-root ""
  gnus-sum-thread-tree-single-leaf "|_ "
  gnus-sum-thread-tree-vertical "|")

(setq gnus-thread-sort-functions
      '(
        (not gnus-thread-sort-by-date)
        (not gnus-thread-sort-by-number)
        ))

; NO 'passive
(setq gnus-use-cache t)
(setq gnus-use-adaptive-scoring t)
(setq gnus-save-score t)
(add-hook 'mail-citation-hook 'sc-cite-original)
(add-hook 'message-sent-hook 'gnus-score-followup-article)
(add-hook 'message-sent-hook 'gnus-score-followup-thread)
; @see http://stackoverflow.com/questions/945419/how-dont-use-gnus-adaptive-scoring-in-some-newsgroups
(setq gnus-parameters
      '(("nnimap.*"
         (gnus-use-scoring nil))
        ))

(defvar gnus-default-adaptive-score-alist
  '((gnus-kill-file-mark (from -10))
    (gnus-unread-mark)
    (gnus-read-mark (from 10) (subject 30))
    (gnus-catchup-mark (subject -10))
    (gnus-killed-mark (from -1) (subject -30))
    (gnus-del-mark (from -2) (subject -15))
    (gnus-ticked-mark (from 10))
    (gnus-dormant-mark (from 5))))

(setq  gnus-score-find-score-files-function
       '(gnus-score-find-hierarchical gnus-score-find-bnews bbdb/gnus-score)
       )

;; BBDB: Address list
(when (file-exists-p "/usr/share/emacs/site-lisp/bbdb")
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/bbdb")
  (require 'bbdb)
  (bbdb-initialize 'message 'gnus 'sendmail)
  (setq bbdb-file "~/bbdb.db")
  (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
  (setq bbdb/mail-auto-create-p t
        bbdb/news-auto-create-p t)
  (defvar bbdb-time-internal-format "%Y-%m-%d"
    "The internal date format.")
  ;;;###autoload
  (defun bbdb-timestamp-hook (record)
    "For use as a `bbdb-change-hook'; maintains a notes-field called `timestamp'
    for the given record which contains the time when it was last modified.  If
    there is such a field there already, it is changed, otherwise it is added."
    (bbdb-record-putprop record 'timestamp (format-time-string
                                             bbdb-time-internal-format
                                             (current-time))))
    )


(add-hook 'message-mode-hook
          '(lambda ()
             (flyspell-mode t)
             (local-set-key "<TAB>" 'bbdb-complete-name)))

;; Fetch only part of the article if we can.  I saw this in someone
;; else's .gnus
(setq gnus-read-active-file 'some)

;; Tree view for groups.  I like the organisational feel this has.
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Threads!  I hate reading un-threaded email -- especially mailing
;; lists.  This helps a ton!
(setq gnus-summary-thread-gathering-function
      'gnus-gather-threads-by-subject)

;; Also, I prefer to see only the top level message.  If a message has
;; several replies or is part of a thread, only show the first
;; message.  'gnus-thread-ignore-subject' will ignore the subject and
;; look at 'In-Reply-To:' and 'References:' headers.
(setq gnus-thread-hide-subtree t)
(setq gnus-thread-ignore-subject t)

; Personal Information
(setq user-full-name "My Name"
      user-mail-address "username@gmail.com"
      ;message-generate-headers-first t
      )

;; Change email address for work folder.  This is one of the most
;; interesting features of Gnus.  I plan on adding custom .sigs soon
;; for different mailing lists.
;; Usage, FROM: My Name <work>
(setq gnus-posting-styles
      '((".*"
     (name "My Name"
          (address "username@gmail.com"
                   (organization "")
                   (signature-file "~/.signature")
                   ("X-Troll" "Emacs is better than Vi")
                   )))))

; You need install the command line brower 'w3m' and Emacs plugin 'w3m'
(setq mm-text-html-renderer 'w3m)

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "username@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-local-domain "homepc")
;http://www.gnu.org/software/emacs/manual/html_node/gnus/_005b9_002e2_005d.html
(setq gnus-use-correct-string-widths nil)
(gnus-compile)

;; The ~/.authinfo.gpg

;machine imap.gmail.com login username@gmail.com password my-secret-password port 993
;machine smtp.gmail.com login username@gmail.com password my-secret-password port 587

;; Please note .authinfo.gpg is a encrypted file. You must use Emacs to edit it. Emacs will do the encryption/descryption automatically. See http://emacswiki.org/emacs/EasyPG for technical details. 
