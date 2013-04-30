#!/bin/bash
#
# A simple dmenu session script.

DMENU='dmenu.sh'
LOCK='xscreensaver-command -lock'

choice=$(echo -e "lock\nsuspend\nhibernate\nshutdown\nreboot\ndisplay off" | $DMENU -p "session:")

case "$choice" in
    lock) $LOCK & ;;

    suspend) $LOCK && dbus-send --system --print-reply \
        --dest="org.freedesktop.UPower" /org/freedesktop/UPower \
        org.freedesktop.UPower.Suspend & ;;

    hibernate) $LOCK && dbus-send --system --print-reply \
        --dest="org.freedesktop.UPower" /org/freedesktop/UPower \
        org.freedesktop.UPower.Hibernate & ;;

    shutdown) dbus-send --system --print-reply \
        --dest="org.freedesktop.ConsoleKit" /org/freedesktop/ConsoleKit/Manager \
        org.freedesktop.ConsoleKit.Manager.Stop & ;;

    reboot) dbus-send --system --print-reply \
        --dest="org.freedesktop.ConsoleKit" /org/freedesktop/ConsoleKit/Manager \
        org.freedesktop.ConsoleKit.Manager.Restart & ;;

    display\ off) sleep 1 && xset dpms force off && $LOCK & ;;
esac
