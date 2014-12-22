;;; mpd
(load-module "mpd")
(setf *mpd-modeline-fmt* "%S [%s;%r]: %a - %t (%n/%p)")
(setf *mpd-map*
      (let ((m (make-sparse-keymap)))
        (define-key m (kbd "SPC") "mpd-toggle-pause")
        (define-key m (kbd "s") "mpd-toggle-random")
        (define-key m (kbd "r") "mpd-toggle-repeat")
        (define-key m (kbd "S") "mpd-current-song")
        (define-key m (kbd "P") "mpd-play")
        (define-key m (kbd "q") "mpd-browse-playlist")
        (define-key m (kbd "o") "mpd-stop")
        (define-key m (kbd "n") "mpd-next")
        (define-key m (kbd "p") "mpd-prev")
        (define-key m (kbd "c") "mpd-clear")
        (define-key m (kbd "x") "mpd-connect")
        (define-key m (kbd "k") "mpd-kill")
        (define-key m (kbd "u") "mpd-update")
        (define-key m (kbd "a") "mpd-search-and-add-artist")
        (define-key m (kbd "z") "mpd-playlist")
        (define-key m (kbd "v") "mpd-set-volume")
        (define-key m (kbd "e") "mpd-volume-up")
        (define-key m (kbd "d") "mpd-volume-down")
        (define-key m (kbd "S") '*mpd-search-map*)
        (define-key m (kbd "b") '*mpd-browse-map*)
        (define-key m (kbd "A") '*mpd-add-map*)
        m))
(define-key *top-map* (kbd "s-m") '*mpd-map*)
