#!/bin/sh
if test $# -eq 1; then
	ERL_CRASH_DUMP_SECONDS=0 erl -pa $MD/erlang -noshell -eval "$1" -s init stop
	exit 0
fi

expr=`zenity --width 350 --entry --text "Please input a expression" \
div618\(\) stopLoss\(\) winG\(\) his\(\) win-ssq\(\) hit_ssq\(\)`

if [ $? -eq 0 ]
then
res=$(ERL_CRASH_DUMP_SECONDS=0 erl -noshell -eval "user_default:$expr" -s init stop)
zenity --info --text="$res"
fi
