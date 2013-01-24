exe=`cat /home/sw2wolf/.dmenu.mnu | dmenu -p run: -l 20 -fn '-*-terminus-*-r-*-*-*-*-*-*-*-*-*-*' -nb '#000000' -nf '#FFFFFF'` && eval "exec $exe"
