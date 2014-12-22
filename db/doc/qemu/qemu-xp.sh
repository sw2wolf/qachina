#!/bin/sh
if [ `pgrep qemu` ]; then
    echo "QEMU is already running..."
    exit
fi
qemu -hda winxp.qcow2 -m 192 -kernel-kqemu -enable-kqemu -localtime -net nic -net user -nographic -daemonize -redir tcp:3389::3389 -localtime
