(add-to-list 'load-path "~/.emacs.d/emms-3.0")

(require 'emms-setup)
(setq emms-player-list '(emms-player-mpd emms-player-mplayer))

(emms-all) ;(emms-standard)
(emms-default-players)

(setq emms-stream-default-action "play")

;; coding settings
(setq emms-info-mp3info-coding-system 'gbk
	  emms-cache-file-coding-system 'utf-8
	  ;; emms-i18n-default-coding-system '(utf-8 . utf-8)
)
;; Show the current track each time EMMS
;; starts to play a track with "播放 : "
(add-hook 'emms-player-started-hook 'emms-show)
(setq emms-show-format "播放: %s")

;; 默认的播放目录
(setq emms-source-file-default-directory "~/music")
(setq emms-playlist-buffer-name "音乐")

;; mode line format
(setq emms-mode-line-format "[ %s ]"
      emms-lyrics-display-format "%s"
      emms-playing-time-display-format "%s")

;; global key-map
;; all global keys prefix is C-c e
;; compatible with emms-playlist mode keybindings
;; you can view emms-playlist-mode.el to get details about
;; emms-playlist mode keys map
(global-set-key (kbd "C-c e s") 'emms-stop)
(global-set-key (kbd "C-c e e") 'emms-pause)
;; (global-set-key (kbd "C-c e n") 'emms-next)
;; (global-set-key (kbd "C-c e p") 'emms-previous)
(global-set-key (kbd "C-c e f") 'emms-show)
;; (global-set-key (kbd "C-c e >") 'emms-seek-forward)
;; (global-set-key (kbd "C-c e <") 'emms-seek-backward)
;; ;; these keys maps were derivations of above keybindings
(global-set-key (kbd "C-c e S") 'emms-start)
(global-set-key (kbd "C-c e g") 'emms-playlist-mode-go)
;; (global-set-key (kbd "C-c e t") 'emms-play-directory-tree)
;; (global-set-key (kbd "C-c e h") 'emms-shuffle)
(global-set-key (kbd "C-c e o") 'emms-play-file)
(global-set-key (kbd "C-c e l") 'emms-play-playlist)
;; (global-set-key (kbd "C-c e r") 'emms-toggle-repeat-track)
;; (global-set-key (kbd "C-c e R") 'emms-toggle-repeat-playlist)
;; (global-set-key (kbd "C-c e u") 'emms-score-up-playing)
;; (global-set-key (kbd "C-c e d") 'emms-score-down-playing)
;; (global-set-key (kbd "C-c e o") 'emms-score-show-playing)
