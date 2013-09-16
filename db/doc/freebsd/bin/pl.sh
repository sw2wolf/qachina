#!/bin/sh

if test $# -ge 1; then
    swipl -q -f $MD/prolog/money.pl -g "$@" -t halt
    exit 0
fi

expr=`zenity --width 350 --entry --text "Please input a prolog predicate" \
div618\(\) stopLoss\(\) winG\(\) his win_ssq\(\) hit_ssq\(\)`

if [ $? -eq 0 ]; then 
    res=$(swipl -q -f $MD/prolog/money.pl -g "$expr" -t halt)
    zenity --info --text="$res"
fi
