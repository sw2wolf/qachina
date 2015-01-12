;; load and fire
(add-to-list 'load-path "~/test/mu/mu4e/")

(require 'mu4e)
(require 'sendmail)
(require 'org-mu4e)

;; hooks
(add-hook 'mu4e-compose-mode-hook
          (defun my-compose-stuff ()
            (set-fill-column 72)
            (flyspell-mode)))

;; my e-mail addresses
(setq mu4e-user-mail-address-list '("z_axis@163.com"
                                    "czsq888@163.com"
                                    "midas_z@163.com"))

;; reply attribution line
(setq message-citation-line-format "On %a, %b %d %Y, %N wrote:")
(setq message-citation-line-function 'message-insert-formatted-citation-line)

;; general settings
(setq mail-user-agent 'mu4e-user-agent                   ; mu4e as default mail agent
      mu4e-attachment-dir "~/download"                   ; put attachements in download dir
      mu4e-get-mail-command "offlineimap"                ; fetch email with offlineimap
      mu4e-confirm-quit nil                              ; don't ask me to quit
      mu4e-headers-skip-duplicates t                     ; skip duplicate email, great for gmail
      mu4e-headers-date-format "%d %b, %Y at %H:%M"      ; date format
      mu4e-headers-leave-behavior 'apply                 ; apply all marks at quit
      mu4e-html2text-command "w3m -dump -T text/html"    ; html to text
      mu4e-compose-dont-reply-to-self t                  ; don't reply to myself
      mu4e-compose-complete-only-personal t              ; only personal messages get in the address book
      mu4e-use-fancy-chars t                             ; use fancy characters
      mu4e-compose-signature "e^(PI.I) + 1 = 0"          ; signature
      message-kill-buffer-on-exit t                      ; don't keep message buffers around
      smtpmail-queue-mail nil                            ; start in non queue mode
)

;; maildir locations
(setq mu4e-maildir "/home/sw2wolf/mail"
      mu4e-sent-folder "/sw2wolf/sent"
      mu4e-drafts-folder "/sw2wolf/drafts"
      mu4e-trash-folder "/sw2wolf/trash"
      mu4e-refile-folder "/sw2wolf/archive"
      smtpmail-queue-dir   "~/mail/queue/cur")

;; sending mail
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-smtp-service 587)

;; set the archive according to the mailbox
(setq mu4e-refile-folder
      (lambda (msg)
        (if msg
            (let ((account (nth 1 (split-string (mu4e-message-field msg :maildir) "/"))))
              (format "/%s/archive" account)))))

;; set the trash according to the mailbox
(setq mu4e-trash-folder
      (lambda (msg)
        (if msg
            (let ((account (nth 1 (split-string (mu4e-message-field msg :maildir) "/"))))
              (format "/%s/trash" account)))))

;; multiple accounts
(setq sw2wolf-mu4e-account-alist
      '(("z_axis"
         (user-mail-address "z_axis@163.com")
         (mu4e-sent-folder "/z_axis/sent")
         (mu4e-drafts-folder "/z_axis/drafts")
         (smtpmail-smtp-server "smtp.163.com")
         (smtpmail-smtp-user "z_axis@163.com"))
        ("czsq"
         (user-mail-address "czsq888@163.com")
         (mu4e-sent-folder "/czsq/sent")
         (mu4e-drafts-folder "/czsq/drafts")
         (smtpmail-smtp-server "smtp.163.com")
         (smtpmail-smtp-user "czsq@163.com"))
        ("midas"
         (user-mail-address "midas_z@163.com")
         (mu4e-sent-folder "/midas/sent")
         (mu4e-drafts-folder "/midas/drafts")
         (smtpmail-smtp-server "smtp.163.com")
         (smtpmail-smtp-user "midas_z@163.com"))))

(defun sw2wolf-mu4e-set-account ()
  "Set the account for composing a message by looking at the maildir"
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-msg-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var)) sw2wolf-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) sw2wolf-mu4e-account-alist)
                             nil t nil nil (caar sw2wolf-mu4e-account-alist))))
         (account-vars (cdr (assoc account sw2wolf-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars))))
(add-hook 'mu4e-compose-pre-hook 'sw2wolf-mu4e-set-account)

;; headers in the overview
(setq mu4e-headers-fields
  '((:maildir       .  24)
    (:date          .  24)
    (:flags         .   6)
    (:from          .  24)
    (:subject       .  nil)))

;; bookmarks
(setq mu4e-bookmarks 
  '(("flag:unread AND NOT maildir:/gibbon/trash AND NOT maildir:/sw2wolf/trash AND NOT maildir:/bread-and-pepper/trash" "All new messages"  ?u)
    ("maildir:/z_axis/inbox"     "z_axis's inbox"   ?g)
    ("maildir:/czsq/inbox"       "czsq's inbox"     ?w)
    ("maildir:/midas/inbox"      "midas's inbox"    ?b)
    ("date:today..now"           "Today's messages"       ?t)
    ("flag:flagged"              "Flagged messages"       ?f)))

;; shortcuts
(setq mu4e-maildir-shortcuts
       '(("/z_axis/inbox"    . ?i)
         ("/czsq/inbox"      . ?I)
         ("/czsq/archive"    . ?a)
         ("/z_axis/archive"  . ?A)
         ("/czsq/sent"       . ?s)
         ("/z_axis/sent"     . ?S)
         ("/czsq/trash"      . ?t)
         ("/z_axis/trash"    . ?T)))

(provide 'sw2wolf-mu4e)
