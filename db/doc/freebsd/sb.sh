#!/bin/sh

MONEY=$MD/money/money.lisp
SB=$HOME/sbcl/bin/sbcl
if test $# -eq 1; then
  $SB --noinform --non-interactive --no-userinit --load $MONEY --eval "$1"
  exit 0
fi

expr=`zenity --width 350 --entry --text "Please input a CL expression" \
\(\) \(apropos\ \"\"\) \(m:div618\) \(m:stopLoss\) \(m:winG\) \(m:his\) \(m:win-ssq\) \(m:hit-ssq\)`

if [ $? -eq 0 ]
then
  res=$($SB --noinform --non-interactive --no-userinit --load $MONEY --eval "$expr")
  zenity --info --text="$res"
fi
