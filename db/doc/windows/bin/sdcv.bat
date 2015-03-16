@echo off
setlocal

zenity --width 350 --entry --text "word:" > "%~d0%~p0\zenity.tmp"
set /p OUTPUT= < "%~d0%~p0\zenity.tmp"

REM c:/app/sdcv/sdcv --utf8-output --data-dir c:/app/sdcv/stardict-stardict1.3-2.4.2/ -n %OUTPUT% > "%~d0%~p0\zenity.tmp"
c:/app/sdcv/sdcv --data-dir c:/app/sdcv/stardict-stardict1.3-2.4.2/ -n %OUTPUT% > "%~d0%~p0\zenity.tmp"
set OUTPUT=
for /f "delims=" %%i in (%~d0%~p0\zenity.tmp) do set OUTPUT=%OUTPUT%%%i
del "%~d0%~p0\zenity.tmp"

zenity --info --text="%OUTPUT%"
