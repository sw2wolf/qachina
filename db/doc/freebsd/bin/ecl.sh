#!/bin/sh

MONEY=$MD/money/money.lisp
if test $# -eq 1; then
  ecl -norc -q -load $MONEY -eval "$1" -eval "(quit)"
  exit 0
fi

expr=`zenity --width 350 --entry --text "Please input a CL expression" \
\(\) \(apropos\ \"\"\) \(m:div618\) \(m:stopLoss\) \(m:winG\) \(m:his\) \(m:win-ssq\) \(m:hit-ssq\)`

if [ $? -eq 0 ]
then
  res=$(ecl -norc -q -load $MONEY -eval "$expr" -eval "(quit)")
  zenity --info --text="$res"
fi
