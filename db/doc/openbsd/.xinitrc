#!/bin/sh
#
# ~~/.xinitrc
#
# Executed by startx (run your window manager from here)
#
export LANG=zh_CN.UTF-8
export LC_CTYPE=en_US.UTF-8
#export LC_CTYPE=zh_CN.UTF-8
export LC_MESSAGES=zh_CN.UTF-8
#export LC_ALL=zh_CN.UTF-8
export XIM_PROGRAM=/usr/local/bin/scim
export XMODIFIERS=@im=scim
export GTK_IM_MODULE=scim
export QT_IM_MODULE=scim

xset -dpms
xset s 2400 600
xset m 1/1 1 #disable X's own mouse acceleration
xset +fp /usr/X11R6/lib/X11/fonts/myfonts/ 

xsetroot -cursor_name left_ptr
xsetroot -solid black

xmodmap ~/.Xmodmap

numlockx &
scim -d &

case $1 in
   stumpwm)
      exec /usr/local/bin/stumpwm
      ;;
   fvwm)
      exec /usr/X11R6/bin/fvwm -f ~/.fvwm/.fvwm2rc
      ;;
    *)
      exec /usr/local/bin/openbox-session
      ;;
esac
