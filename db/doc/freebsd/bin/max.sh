#!/bin/sh
if test $# -eq 1; then
	maxima --very-quiet -p $MD/money/money.lisp --batch-string="$1"
	exit 0
fi

expr=`zenity --width 350 --entry --text "Please input a CL expression" \
\(m:div618\) \(m:stopLoss\) \(m:winG\) \(m:his\) \(m:win_ssq\) \(m:hit_ssq\)`

if [ $? -eq 0 ]
then
res=$(maxima --very-quiet -p $MD/money/money.lisp --batch-string="$expr")
zenity --info --text="$res"
fi
