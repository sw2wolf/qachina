#!/bin/sh
if test $# -eq 1; then
	guile -q -l $MD/scheme/money-guile.scm -c "$1"
	exit 0
fi

expr=`zenity --width 350 --entry --text "Please input a CL expression" \
\(\) \(apropos\ \"\"\) \(m:div618\) \(m:stopLoss\) \(m:winG\) \(m:his\) \(m:win-ssq\) \(m:hit-ssq\)`

if [ $? -eq 0 ]
then
    res=$(guile -q -l $MD/scheme/money-guile.scm -c "$expr")
    zenity --info --text="$res"
fi
