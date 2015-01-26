#!/bin/sh
if [ `pgrep qemu` ]; then
    echo "QEMU is already running..."
    exit
fi
qemu-system-i386 -hda /mnt/D/winxp.qcow2 -m 192 -localtime \
	-net nic -net user


#-net nic,model=e1000 -net user
#-nographic -daemonize -redir tcp:3389::3389
