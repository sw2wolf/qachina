#!/bin/sh
#word=`echo ""| dmenu -p word: -b -nb '#000000' -nf '#FFFFFF' -fn '-*-simsun-medium-r-normal-*-16-*-*-*-*-*-iso10646-1'`
word=`zenity --width 350 --entry --text "Please input a word"`

if [ $? -eq 0 ]
then
    #res=$(sdcv -n $word)
	res=$(wn $word -over)
    zenity --info --text="$res"
fi
