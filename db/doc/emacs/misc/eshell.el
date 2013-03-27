(defun my-buffer-face-mode-courrier ()
    (interactive)
    (setq buffer-face-mode-face '(:family "Courrier"))
    (buffer-face-mode))

(add-hook 'eshell-mode-hook 'my-buffer-face-mode-courrier)
