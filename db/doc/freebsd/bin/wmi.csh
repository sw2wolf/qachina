#!/bin/csh

(sleep 3; kill -INT $$) & 
onintr bye

echo "please choose WM(1/2)"
echo "----------------------"
echo "1:stumpwm-clisp with MPD"
echo "2:xmonad"
echo "3:dwm"
echo "9:console"
echo "----------------------"

set req = $<
switch ($req)
    case [1]:
		exec xinit clisp
		breaksw
	case [2]:
        exec xinit xmonad
        breaksw
	case [3]:
        exec xinit dwm
        breaksw
	case [9]:
        breaksw
    default:
		exec xinit
		breaksw
endsw
exit 0

bye:
    exec xinit
	exit 1

#set i = 0
# while ($i < 9)
#    echo $i
#    @ i = $i + 1
# end
