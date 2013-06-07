#!/bin/sh

BASE=/media/D/qachina/db/doc/money

goal=`zenity --width 350 --entry --text "Please input goal" \
div618\(,\). stopLoss\(,\). his. \
win_ssq\(,\'\',\'\'\). hit_ssq\(\'\',\'\'\).`

if [ $? -eq 0 ]
then
res=`swipl -q -f $BASE/money.pl -g "$goal" -t halt`
zenity --info --text="$res"
fi
