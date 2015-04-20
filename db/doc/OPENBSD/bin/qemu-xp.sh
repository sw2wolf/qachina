#!/bin/sh
if [ `pgrep qemu` ]; then
    echo "QEMU is already running..."
    exit
fi
qemu-system-i386 -hda /mnt/D/winxp.qcow2 -m 192 -localtime   \
    -net nic,vlan=0,macaddr=52:54:00:12:34:22,model=e1000 -net user \
	-nographic -redir tcp:3389::3389
    #-usbdevice tablet -vnc 0.0.0.0:1 \
    #-monitor stdio

#-net nic,vlan=0,macaddr=52:54:00:12:34:22,model=rtl8139 -net user \
#-monitor telnet::4444,server,nowait
