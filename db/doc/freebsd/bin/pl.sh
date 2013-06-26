#!/bin/sh

BASE=/media/D/qachina/db/doc/money

goal=`zenity --width 350 --entry --text "Please input goal" \
div618\(,\) stopLoss\(,\) winG\(,,\)  hit_ssq\(\'\',\'\'\) \
his win_ssq\(,\'\',\'\'\)`

if [ $? -eq 0 ]
then
#res=`swipl -q -f $BASE/money.pl -g "$goal" -t halt`
#res=`gprolog --consult-file $BASE/money.pl --query-goal $goal,halt`
res=`gprolog --init-goal "consult('$BASE/money.pl'),$goal,halt" `
zenity --info --text="$res"
fi
