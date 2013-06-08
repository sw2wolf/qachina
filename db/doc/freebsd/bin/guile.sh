#!/bin/sh
expr=`zenity --width 350 --entry --text "Please input a expression" \
\(div618\) \(stopLoss\) \(his\) \(win-ssq\) \(hit-ssq\)`

if [ $? -eq 0 ]
then
res=$(guile -q -l $MD/money/money.scm -c "(chdir \"$MD/money\") $expr")
zenity --info --text="$res"
fi
