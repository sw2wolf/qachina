#!/bin/sh
res=$(clisp -q -q -modern -ansi -x "$*")

if [ $? -eq 0 ]
then
zenity --info --text="$res"
else
zenity --error --text="$res"
fi
