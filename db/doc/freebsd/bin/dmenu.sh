#!/bin/sh
#exe=`cat /home/sw2wolf/.dmenu.mnu | dmenu -p run: -l 20 -fn '-*-simsun-medium-r-normal-*-12-*-*-*-*-*-iso10646-1' -nb '#000000' -nf '#FFFFFF'` && eval "exec $exe"
exe=`cat /home/sw2wolf/.dmenu.mnu | dmenu -p run: -l 20 -nb '#000000' -nf '#FFFFFF'` && eval "exec $exe"
