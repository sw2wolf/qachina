#!/bin/sh

SWI="/home/sw2wolf/swi/bin/swipl"

if test $# -ge 1; then
    $SWI -q -f $MD/prolog/money.pl -g "$@" -t halt
    exit 0
fi

expr=`zenity --width 350 --entry --text "Please input a prolog predicate" \
div618\(\) stopLoss\(\) winG\(\) his hit_ssq\(\)`

if [ $? -eq 0 ]; then 
    res=$($SWI -q -f $MD/prolog/money.pl -g "$expr" -t halt)
    zenity --info --text="$res"
fi
