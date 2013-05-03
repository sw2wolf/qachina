(defun radio ()
   (interactive)
   (let ((filename
         (ido-completing-read "which radio?: "
                              (directory-files
                               "/home/sw2wolf/radio/"
                               nil
                               "\\.pls$\\|\\.asx$"))))
	 (add-to-list 'warning-suppress-types '(undo discard-info))
     (async-shell-command
      (concat "mplayer -nocache -playlist /home/sw2wolf/radio/" filename) "*mplayer*" )
     (message "choosen: %s" filename)))
