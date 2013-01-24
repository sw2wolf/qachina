#!/bin/bash
if [ `pidof qemu-kvm` ]; then
    echo "KVM is already running..."
    exit
fi
/usr/bin/qemu-kvm -enable-kvm -m 192 -soundhw es1370 -hda /media/E/winxp.img -hdb /dev/sda7 -localtime -net nic -net user -nographic -daemonize -redir tcp:3389::3389 -boot c -k en-us &
