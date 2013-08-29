#!/bin/sh
if test $# -eq 1; then
	erl -pa $MD/erlang -noshell -eval $1 -s init stop
	exit 0
fi

expr=`zenity --width 350 --entry --text "Please input a expression" \
user_default:div618\(\) user_default:stopLoss\(\) user_default:winG\(\) \
user_default:his\(\) user_default:win-ssq\(\) user_default:hit-ssq\(\)`

if [ $? -eq 0 ]
then
res=$(erl -pa $MD/erlang -noshell -eval "$expr" -s init stop)
zenity --info --text="$res"
fi
