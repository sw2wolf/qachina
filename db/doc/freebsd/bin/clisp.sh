#!/bin/sh
#expr=`echo ""| dmenu -p Eval: -b -nb '#000000' -nf '#FFFFFF' -fn '-*-simsun-medium-r-normal-*-16-*-*-*-*-*-iso10646-1'`
expr=`zenity --width 350 --entry --text "Please input a CL expression" \
\(stopLoss\) \(div618\) \(his\) \(win-ssq\) \(hit-ssq\)`

if [ $? -eq 0 ]
then
res=$(clisp -norc -q -q -i $MD/money/util.lisp -x "(cd \"$MD/money\") $expr")
zenity --info --text="$res"
fi
