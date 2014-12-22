#!/bin/sh
#
# ~~/.xinitrc
#
# Executed by startx (run your window manager from here)
#
LOCALE="zh_CN.UTF-8"
export XIM_PROGRAM=/usr/bin/fcitx
export XIM=fcitx
export XMODIFIERS=@im=fcitx
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx

#improving Flash performance
export OPERAPLUGINWRAPPER_PRIORITY=0
export OPERA_KEEP_BLOCKED_PLUGIN=0
#GDK_NATIVE_WINDOWS=1

xset -dpms
xset s 1800 600
xsetroot -cursor_name left_ptr
xsetroot -solid black
xmodmap -e "pointer = 3 2 1 4 5 6 7 8 9"
xmodmap ~/.Xmodmap
#xrdb ~/.Xdefaults

numlockx &

case $1 in
    sawfish)
      exec sawfish
      ;;
    *)
      exec /usr/local/bin/stumpwm
      ;;
esac
