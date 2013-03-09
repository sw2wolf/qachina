#!/bin/csh
echo "please choose WM(1/2)"
echo "----------------------"
echo "1:stumpwm-clisp"
echo "2:stumpwm-ccl"
echo "3:xmonad"
echo "----------------------"

set req = $<
switch ($req)
    case [2]:
		xinit ccl
		breaksw
    case [3]:
		xinit xmonad
		breaksw
    default:
        xinit
		breaksw
endsw

#set i = 0
# while ($i < 9)
#    echo $i
#    @ i = $i + 1
# end
