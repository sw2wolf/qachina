;;Do some key re-mapping; it is crucial that this get run first, because otherwise
;;the remapping later on of Insert and less to the prefix key simply will not work.
;(run-shell-command "xmodmap -quiet ~/.Xmodmap")

;;Apparently modifies some low-level GUI bits of X.
;(run-shell-command "xrdb -load ~/.Xresources -quiet")

;;Change the background and pointer in X
;(run-shell-command "xsetroot -cursor_name left_ptr -gray -fg darkgreen -bg black -name root-window")
