#!/bin/sh
exec >.xsession-errors 2>&1

#
# ~~/.xinitrc
#

#pgrep aucat || /usr/bin/aucat -l &
#xset b off
#xset r rate 250 80
#ssh-agent cwm

xset -dpms
xset s 2400 600
xset m 1/1 1 #disable X's own mouse acceleration

xset fp+ /usr/local/lib/X11/fonts/wqy/
xset fp rehash

# trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype percent --width 5 --transparent true --tint 0x000000 --height 12 &
# xscreensaver -no-splash &

xsetroot -cursor_name left_ptr
xsetroot -solid black
#setxkbmap -option terminate:ctrl_alt_bksp 
#xclock -geometry -0+0 -d -strftime '%H:%M %b %d %Y' -update 1 &

export LC_ALL=zh_CN.UTF-8
export LANG=zh_CN.UTF-8

xmodmap ~/.Xmodmap
#mixer vol 95 pcm 95
#export XIM_PROGRAM=/usr/local/bin/fcitx
#export XMODIFIERS=@im=fcitx
#export GTK_IM_MODULE=fcitx
#export QT_IM_MODULE=fcitx
#fcitx &

case $1 in
	clisp)
		#export MPD=YES
	    #/usr/local/etc/rc.d/musicpd onestart
		exec $HOME/bin/stumpwm-clisp-norc
        ;;
    sbcl)
		#sbcl --eval '(require (quote stumpwm))' --eval '(stumpwm:stumpwm)'
        exec $HOME/bin/stumpwm-sbcl
		;;
	cwm)
		exec cwm
		;;
    dwm)
        #xterm -e gdb --quiet -nx $HOME/test/dwm-dbg/dwm -ex run
        xterm -e gdb --quiet -nx -x ~/script.gdb
		;;
    *)
		#exec $HOME/bin/stumpwm
		exec $HOME/bin/dwm
		;;
esac
