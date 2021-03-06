#!/bin/sh

MONEY=$MD/scheme/money-guile.scm
if test $# -eq 1; then
  guile -q -l $MONEY -c "$1"
  exit 0
fi

expr=`zenity --width 350 --entry --text "Please input a CL expression" \
\(\) \(apropos\ \"\"\) \(div618\) \(stopLoss\) \(winG\) \(his\) \(win_ssq\) \(hit_ssq\)`

if [ $? -eq 0 ]
then
  res=$(guile -q -l $MONEY -c "$expr")
  zenity --info --text="$res"
fi
