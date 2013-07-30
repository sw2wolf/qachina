#!/bin/sh
if test $# -eq 1; then
    clisp -norc -q -q -i $MD/money/money.lisp -x "$1"
	exit 0
fi

#expr=`echo ""| dmenu -p Eval: -b -nb '#000000' -nf '#FFFFFF' -fn '-*-simsun-medium-r-normal-*-16-*-*-*-*-*-iso10646-1'`
expr=`zenity --width 350 --entry --text "Please input a CL expression" \
\(m:stopLoss\) \(m:div618\) \(m:winG\) \(m:his\) \(m:win-ssq\) \(m:hit-ssq\)`

if [ $? -eq 0 ]
then
    res=$(clisp -norc -q -q -i $MD/money/money.lisp -x "$expr")
    zenity --info --text="$res"
fi
