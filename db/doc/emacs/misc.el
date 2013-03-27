;输入 M-X erc-bitlbee 的时候就自动连上我机器上的 Bitlbee 服务器。 Bitlbee 是一个 GTalk/MSN/xxxx 转 IRC 的程序，使我可以通过 IRC 客户端登 录这些 IM 协议。
;; (defun erc-bitlbee ()
;;   (interactive)
;;   (erc :server "127.0.0.1" :port 6667 :nick erc-nick :password bitlbeepw))
;; (defun bitlbee-identify ()
;;    "If we're on the bitlbee server, send the identify command to
;;  the &bitlbee channel."
;;    (when (and (string= "127.0.0.1" erc-session-server)
;;               (string= "&bitlbee" (buffer-name)))
;;      (erc-message "PRIVMSG" (format "%s identify %s"
;;                                     (erc-default-target)
;;                                     bitlbeepw))))

;; (defadvice erc-cmd-IGNORE (after ignore-replys-to (&optional user) activate)
;;   "After every ignore, copy the list `erc-ignore-list' to
;; `erc-ignore-reply-list'. When I ignore someone, I want them *gone*."
;;   (erc-with-server-buffer (setq erc-ignore-reply-list erc-ignore-list)))

;; (defadvice erc-cmd-UNIGNORE (after ignore-replys-to (&optional user) activate)
;;   "In case of mistakes made with /ignore."
;;   (erc-with-server-buffer (setq erc-ignore-reply-list erc-ignore-list)))

;; (defun erc-cmd-OPME ()
;;   "tell chanserv to op me (from: http://paste.lisp.org/display/97466)"
;;   (interactive)
;;   (erc-message "PRIVMSG"
;; 	       (format "chanserv op %s %s"
;; 		       (erc-default-target)
;; 		       (erc-current-nick)) nil))

;; (defun erc-cmd-DEOPME ()
;;   "deop myself (from: http://paste.lisp.org/display/97466)"
;;   (interactive)
;;   (erc-cmd-DEOP (format "%s" (erc-current-nick))))

;显示一个“想”的气泡 ^_^
;; (defun erc-cmd-THINK (&rest line)
;;   (let ((text
;;          (concat ".oO{ "
;;                  (erc-trim-string (mapconcat 'identity line " "))
;;                  " }")))
;;     (erc-send-action (erc-default-target) text)))

;; (defun erc-cmd-HOWMANY (&rest ignore)
;;   "Display how many users (and ops) the current channel has."
;;   (erc-display-message nil 'notice (current-buffer)
;;                        (let ((hash-table (with-current-buffer
;;                                              (erc-server-buffer)
;;                                            erc-server-users))
;;                              (users 0)
;;                              (ops 0))
;;                          (maphash (lambda (k v)
;;                                     (when (member (current-buffer)
;;                                                   (erc-server-user-buffers v))
;;                                       (1+ users))
;;                                     (when (erc-channel-user-op-p k)
;;                                       (1+ ops)))
;;                                   hash-table)
;;                          (format
;;                           "There are %s users (%s ops) on the current channel"
;;                           users ops))))

;/slap 是骂人用的。
;; (defun erc-cmd-SLAP (&rest nick)
;;   (if (not (equal '() nick))
;;       (erc-send-action (erc-default-target)
;;                        (concat "slaps "
;;                                (car nick)
;;                                " around the solar system "
;;                                "-- just out of spite!"))))

;显示一段 Elisp 代码及其返回值。比如输入 /show (+ 1 1) 就会显示
;<Darksair> (+ 1 1) => 2
;; (defun erc-cmd-SHOW (&rest form)
;;   "Eval FORM and send the result and the original form as:
;;  FORM => (eval FORM)."
;;   (let* ((form-string (mapconcat 'identity form " "))
;;          (result
;;           (condition-case err
;;               (eval (read-from-whole-string form-string))
;;             (error
;;              (format "Error: %s" error)))))
;;     (erc-send-message (format "%s => %S" form-string result))))


;; ;; Show info page
;; (defun erc-cmd-INFO (&rest ignore)
;;     "Send current info node."
;;    (unless (get-buffer "*info*")
;;     (error "No *info* buffer"))
;;    (let (output)
;;     (with-current-buffer "*info*"
;;       (let* ((file (file-name-nondirectory Info-current-file))
;;          (node Info-current-node))
;;         (setq output (format "(info \"(%s)%s\") <-- hit C-x C-e to evaluate"
;;                  file node))))
;;     (erc-send-message output)))
