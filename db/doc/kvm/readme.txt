sudo apt-get install gcc libsdl1.2-dev zlib1g-dev libasound2-dev linux-kernel-headers pkg-config libgnutls-dev

./configure --prefix=/usr/local --audio-drv-list="alsa oss" --enable-mixemu
make
sudo make install

sudo modprobe kvm
sudo modprobe kvm-intel  //如果你的是INTEL处理器就用这个
sudo modprobe kvm-amd  //如果你的是AMD处理器就用这个

/usr/local/kvm/bin/qemu-img create -f qcow2 winxp.img 10G

#!/bin/bash
if [ `pidof qemu-system-x86_64` ]; then
    echo "KVM is already running..."
    exit
fi
/usr/local/bin/qemu-system-x86_64  -m 256 -soundhw es1370 -hda /media/E/winxp.img -hdb /dev/sda7 -localtime -net nic -net user -nographic -daemonize -redir tcp:3389::3389 -boot c -k en-us &
#
#/usr/local/bin/qemu-system-x86_64 -M pc -m 256 -soundhw es1370 -hda /media/E/winxp.img -hdb /dev/sda7 -localtime -net nic,model=virtio -net user -nographic -daemonize -vnc :0 -redir tcp:3389::3389 -boot c &
#


#xset s off # 关闭萤幕保护。
xset -dpms # 关闭 DPMS。 
xset s 1800 600
rdesktop localhost:3389 -u sw2wolf -p 123 -g 1440x880 -D -K -r sound:remote
