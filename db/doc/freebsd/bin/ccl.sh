#!/bin/sh
#expr=`echo ""| dmenu -p Eval: -b -nb '#000000' -nf '#FFFFFF' -fn '-*-simsun-medium-r-normal-*-16-*-*-*-*-*-iso10646-1'`
expr=`zenity --width 350 --entry --text "Please input a CL expression" \
\(stopLoss\) \(div618\) \(winG\) \(his\) \(win-ssq\) \(hit-ssq\)`

if [ $? -eq 0 ]
then
#res=$(~/ccl/fx86cl -n -Q --eval "(progn (compile-file \"/media/D/qachina/db/doc/money/util\") (ccl:quit))" )
res=$(~/ccl/fx86cl -n -Q --eval "(progn (load \"/media/D/qachina/db/doc/money/util\") $expr (ccl:quit))" )
zenity --info --text="$res"
fi
