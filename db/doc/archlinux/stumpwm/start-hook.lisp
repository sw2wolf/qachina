(defun my-start-hook ()
      ;; Keep the X cursor out of the way.
      ;; (run-with-timer 5 5 'banish-pointer)
      ;; Change the background and pointer in X
      ;(run-shell-command "xsetroot -cursor_name left_ptr -gray -fg white -bg black -name root-window")
      ;; (run-shell-command "feh --bg-scale /home/tsp/.wmii-3.5/wallpaper/wmii.jpg")
      ;; (run-shell-command "xsetbg /home/enigma/media/pictures/artwork/vintage_wallpaper_blue.png")
      ;; Run unclutter so the mouse hangs around no longer than needed.
      ;(run-shell-command "unclutter -idle 1 -jitter 2 -root")
      ;; I use Xscreensaver as a screensaver. The first line makes sure any running Xscreensaver is killed.
      ;; The second run regardless of the success of the first & starts a background Xscreensaver daemon
      ;; (run-shell-command "xscreensaver-command -exit; killall xscreensaver 2>/dev/null; xscreensaver -no-splash")
      ;; (run-shell-command "dmenu_path")
      (run-shell-command "fcitx")
      (run-shell-command "xterm -e tmux"))
