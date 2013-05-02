#!/bin/csh

(sleep 3; kill -INT $$) & 
onintr bye

echo "please choose WM(1/2)"
echo "----------------------"
echo "1:stumpwm-clisp"
echo "2:stumpwm-ccl"
echo "3:stumpwm-sbcl"
echo "9:console"
echo "----------------------"

set req = $<
switch ($req)
    case [1]:
		xinit clisp
		breaksw
	case [2]:
        xinit ccl
        breaksw
    case [3]:
		xinit sbcl
		breaksw
	case [9]:
        breaksw
    default:
		xinit
		breaksw
endsw
exit 0

bye:
    xinit
	exit 1

#set i = 0
# while ($i < 9)
#    echo $i
#    @ i = $i + 1
# end
