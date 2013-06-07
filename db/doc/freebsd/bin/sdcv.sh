
#!/bin/sh
res=`sdcv -n $1`

if [ $? -eq 0 ]
then
zenity --info --text="$res"
else
zenity --error --text="No input provided"
fi
