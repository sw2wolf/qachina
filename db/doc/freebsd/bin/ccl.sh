#!/bin/sh
if test $# -eq 1; then
	~/ccl/fx86cl -n -Q -l $MD/money/money -e "(progn $1 (ccl:quit))"
	exit 0
fi

expr=`zenity --width 350 --entry --text "Please input a CL expression" \
\(m:div618\) \(m:stopLoss\) \(m:winG\) \(m:his\) \(m:win_ssq\) \(m:hit_ssq\)`

if [ $? -eq 0 ]
then
#res=$(~/ccl/fx86cl -n -Q -e '(progn (compile-file "/media/D/qachina/db/doc/money/money") (ccl:quit))' )
res=$(~/ccl/fx86cl -n -Q -l $MD/money/money -e "(progn $expr (ccl:quit))")
zenity --info --text="$res"
fi
