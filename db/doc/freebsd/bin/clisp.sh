#!/bin/sh
if test $# -eq 1; then
    clisp -norc -q -q -i $MD/money/money.lisp -x "$1"
	exit 0
fi

expr=`zenity --width 350 --entry --text "Please input a CL expression" \
\(\) \(apropos\ \"\"\) \(m:div618\) \(m:stopLoss\) \(m:winG\) \(m:his\) \(m:win_ssq\) \(m:hit_ssq\)`

if [ $? -eq 0 ]
then
    res=$(clisp -norc -q -q -i $MD/money/money.lisp -x "$expr")
    zenity --info --text="$res"
fi
