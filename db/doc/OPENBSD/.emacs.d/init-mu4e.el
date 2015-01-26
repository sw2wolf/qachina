(add-to-list 'load-path "~/RnD/mu/mu4e")
(require 'mu4e)

;(setq mu4e-get-mail-command "true")            ;; Don't get mail, just re-index
(setq
   mu4e-get-mail-command "gm.sh"   ;; or fetchmail, or ...
   mu4e-update-interval 600)       ;; update every 5 minutes
  
;; (add-hook 'mu4e-index-updated-hook  
;;   (defun new-mail-sound ()  
;;     (shell-command "aplay ~/Music/open.wav&")))  

(setq mu4e-attachment-dir "~/download")
(setq mu4e-show-images t)

(setq mail-user-agent 'mu4e-user-agent)      
;; something about ourselves
(setq
   user-mail-address "z_axis@163.com"
   user-full-name  "z_axis"
   mu4e-compose-signature
    (concat "Cheers,\n" "Blah Man\n"))

(setq mu4e-user-mail-address-list '("z_axis@163.com" "czsq888@163.com"))
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/usr/bin/msmtp")

(defun choose-msmtp-account ()
  (if (message-mail-p)
      (save-excursion
        (let*
            ((from (save-restriction
                     (message-narrow-to-headers)
                     (message-fetch-field "from")))
             (account
              (cond
               ((string-match "z_axis@163.com" from) "z_axis")
               ((string-match "czsq888@163.com" from) "czsq"))))
          (setq message-sendmail-extra-arguments (list '"-a" account))))))
(setq message-sendmail-envelope-from 'header)
(add-hook 'message-send-mail-hook 'choose-msmtp-account)

(setq mu4e-maildir       "~/.Mail"             ;; top-level Maildir
      mu4e-sent-folder   "/sent"               ;; folder for sent messages
      mu4e-drafts-folder "/drafts"             ;; unfinished messages
      mu4e-trash-folder  "/trash"              ;; trashed messages
      mu4e-refile-folder "/archive")           ;; saved messages

(setq mu4e-maildir-shortcuts
      '(("/.Received.d/Inbox" . ?i)
        ("/drafts" . ?d)
        ("/sent" . ?s)))

(setq mu4e-headers-fields
      '((:human-date . 11)
        (:flags . 5)
        (:mailing-list . 13)
        (:from-or-to . 20)
        (:subject)))

;; (setq mu4e-org-contacts-file "~/.org/contacts.org")
;; (add-to-list 'mu4e-headers-actions
;;   '("org-contact-add" . mu4e-action-add-org-contact) t)
;; (add-to-list 'mu4e-view-actions
;;   '("org-contact-add" . mu4e-action-add-org-contact) t)

;; add option to view html message in a browser
;; `aV` in view to activate
(add-to-list 'mu4e-view-actions
  '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;(setq mu4e-sent-messages-behavior 'delete)
;; use imagemagick, if available
;; (when (fboundp 'imagemagick-register-types)
;;   (imagemagick-register-types))

(setq mu4e-compose-complete-addresses nil)
(setq mu4e-compose-signature-auto-include nil)

(setq mu4e-hide-index-messages t)

(define-key mu4e-headers-mode-map (kbd "v") 'mu4e~headers-jump-to-maildir)
(define-key mu4e-headers-mode-map (kbd "j") 'next-line)
(define-key mu4e-headers-mode-map (kbd "k") 'previous-line)
(define-key mu4e-headers-mode-map (kbd "r") nil)

;; configuration for sending mail
;; (setq message-send-mail-function 'smtpmail-send-it
;;      smtpmail-stream-type 'starttls
;;      smtpmail-default-smtp-server "smtp.gmail.com"
;;      smtpmail-smtp-server "smtp.gmail.com"
;;      smtpmail-smtp-service 587)

;; https://groups.google.com/d/msg/mu-discuss/dNvZSazm_Ms/p3t9isE6a_kJ

(setq shr-width fill-column)
(setq shr-bullet " ")

(defun shr-html2text ()
  "Replacement for standard html2text using shr."
  (interactive)
  (let ((dom (libxml-parse-html-region (point-min) (point-max))))
    (erase-buffer)
    (shr-insert-document dom)
    (goto-char (point-min))))

(setq mu4e-html2text-command 'shr-html2text)

;; convert html emails properly
;; Possible options:
;;   - html2text -utf8 -width 72
;;   - textutil -stdin -format html -convert txt -stdout
;;   - html2markdown | grep -v '&amp;nbsp_place_holder;' (Requires html2text pypi)
;;   - w3m -dump -cols 80 -T text/html
;;   - view in browser (provided below)
;(setq mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout")

;; spell check
;; (add-hook 'mu4e-compose-mode-hook
;; 	(defun my-do-compose-stuff ()
;; 	   "My settings for message composition."
;; 	   (set-fill-column 72)
;; 	   (flyspell-mode)))

;; (setq 
;;  mu4e-maildir "~/.Maildir"
;;  mu4e-sent-folder "/Sent"
;;  mu4e-drafts-folder "/Drafts"
;;  mu4e-trash-folder "/Trash"
;;  mu4e-refile-folder 'refile-to-old-date-folder)

;
;~/.authinfo file and add the following into it:
;machine smtp.gmail.com login gmailusername password gmailpassword

;You can also encrypt the above file by running:
;gpg --output ~/.authinfo.gpg --symmetric ~/.authinfo

;; [general]
;; ui=TTYUI
;; accounts = Gmail
;; autorefresh = 5
 
;; [Account Gmail]
;; localrepository = Gmail-Local
;; remoterepository = Gmail-Remote
 
;; [Repository Gmail-Local]
;; type = Maildir
;; localfolders = ~/.Mail/xxx@gmail.com
 
;; [Repository Gmail-Remote]
;; type = Gmail
;; remotehost = imap.gmail.com
;; remoteuser = xxx@gmail.com
;; remotepass = ***
;; realdelete = no
;; ssl = yes
;; cert_fingerprint = e227f1d600c0c2f7115775f5cb748805677db57d
;; maxconnections = 1
;; folderfilter = lambda folder: folder not in ['[Gmail]/Trash',
;; 					     '[Gmail]/Spam',
;; 					     '[Gmail]/All Mail',
;; 					     ]

;;; message view action
;; (defun mu4e-msgv-action-view-in-browser (msg)
;;   "View the body of the message in a web browser."
;;   (interactive)
;;   (let ((html (mu4e-msg-field (mu4e-message-at-point t) :body-html))
;;         (tmpfile (format "%s/%d.html" temporary-file-directory (random))))
;;     (unless html (error "No html part for this message"))
;;     (with-temp-file tmpfile
;;       (insert
;;         "<html>"
;;         "<head><meta http-equiv=\"content-type\""
;;         "content=\"text/html;charset=UTF-8\">"
;;         html))
;;       (browse-url (concat "file://" tmpfile))))
;; (add-to-list 'mu4e-view-actions
;;   '("View in browser" . mu4e-msgv-action-view-in-browser) t)

;; (add-hook 'mu4e-index-updated-hook  
;;   (defun new-mail-sound ()  
;;     (shell-command "aplay ~/Music/open.wav&")))

;mu index --maildir=~/.Mail
