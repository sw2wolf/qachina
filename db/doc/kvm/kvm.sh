#!/bin/bash
if [ `pidof qemu-kvm` ]; then
    echo "KVM is already running..."
    exit
fi
/usr/bin/qemu-kvm -enable-kvm -m 192 -soundhw es1370 -hda /home/sw2wolf/winxp.img -hdb /dev/sda7 -localtime -net nic -net user -nographic -daemonize -redir tcp:3389::3389 -boot c -k en-us &

#one of the most often forgotten options of qemu/kvm is the (virtual) disk caching mode, older qemu/kvm had the worst (slowest) cache mode by default.  When you install a new OS, I recommend using cache=unsafe, and for production/testing, cache=none/writeback
