;;;; -*- mode: Emacs-Lisp -*-
;;;;
;;;; Mail configuration

;;; mu4e Configuration
;;; NB: several values come from private.el to protect the innocent

(require 'mu4e)

(setq
    mu4e-maildir       "~/Maildir"   ;; top-level Maildir
	mu4e-sent-folder   "/sent"       ;; folder for sent messages
	mu4e-drafts-folder "/drafts"     ;; unfinished messages
	mu4e-trash-folder  "/trash")      ;; trashed messages

(setq smtpmail-queue-mail nil
      smtpmail-queue-dir  "~/Maildir/queue/cur")

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server smtp.163.com
      smtpmail-smtp-server "smtp.163.com"
      smtpmail-local-domain "163.com")

(setq mu4e-get-mail-command "offlineimap"
      mu4e-html2text-command "html2text -nobs -utf8 -width 72")

(setq mu4e-reply-to-address "z_axis@163.com"
	  mu4e-user-mail-address-list (list "madias_z@163.com" "z_axis@163.com" "czsq888@163.com"))

;; something about ourselves
(setq
     user-mail-address "z_axis@163.com"
	 user-full-name  "(z axis)"
	 message-signature
	 (concat
	  "z axis\n"
	  "Email: z_axis@163.com\n"
	  "Blog: blog.csdn.net/sw2wolf\n"
	  "\n"))

(setq mu4e-headers-date-format "%Y-%m-%d %H:%M"
	  mu4e-confirm-quit nil)
(setq mu4e-headers-fields
      '((:date . 18)
        (:flags . 6)
        (:maildir . 10)
        (:from-or-to . 20)
        (:subject)))

(setq mu4e-bookmarks
      '(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
        ("date:today..now AND NOT flag:trashed AND NOT from:temerson@*" "Today's messages" ?t)
        ("date:2d..now AND NOT flag:trashed AND NOT from:temerson@*" "Last 2 days messages" ?2)
        ("date:..2w AND flag:trashed" "Trashed older than 2w" ?T)))

(setq mu4e-refile-folder
      (lambda (msg)
        (cond
          ;; status messages go to /status
          ((and (mu4e-message-contact-field-matches msg :to *ep-manager-address*)
                (string-match "^Status, " (or (mu4e-message-field msg :subject) "")))
           "/status")
          ;; HealthMiles go to /trash
          ((mu4e-message-contact-field-matches msg :from "HealthMiles") "/trash")
          ;; "Config" errors go to /trash - with various development and QA groups using
          ;; the live error service and their unwillingness to not do it, these become
          ;; meaningless.
          ((and (mu4e-message-contact-field-matches msg :from *ep-error-service-address*)
                (string-match "Config errors" (or (mu4e-message-field msg :subject) "")))
           "/trash")
          ;; BIBFRAME go to /bibframe
          ((mu4e-message-contact-field-matches msg :to "BIBFRAME")
           "/bibframe")
          (t "/archive"))))

(setq mail-user-agent 'mu4e-user-agent)
(setq message-kill-buffer-on-exit t)

;;;; the following was taken from http://www.brool.com/index.php/using-mu4e

(setq mu4e-show-images t)
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;;; message view action
(defun mu4e-msgv-action-view-in-browser (msg)
  "View the body of the message in a web browser."
  (interactive)
  (let ((html (mu4e-msg-field (mu4e-message-at-point t) :body-html))
        (tmpfile (format "%s/%d.html" temporary-file-directory (random))))
    (unless html (error "No html part for this message"))
    (with-temp-file tmpfile
      (insert
       "<html>"
       "<head><meta http-equiv=\"content-type\""
       "content=\"text/html;charset=UTF-8\">"
       html))
    (browse-url (concat "file://" tmpfile))))

(add-to-list 'mu4e-view-actions
             '("View in browser" . mu4e-msgv-action-view-in-browser) t)
