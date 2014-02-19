
######
How to convert VirtualBox VDI image to Qemu-KVM .QCOW image
Convert the VirtualBox image to a raw image format using the following command:
$VBoxManage clonehd "image.vdi" "image.img" --format RAW

Convert convert the raw image to a qcow image using the following command:
$qemu-img convert -f raw -O qcow2 image.img image.qcow

Install:
qemu -m 192 -hda winxp.qcow2 -cdrom winxp.iso -boot d

Test your new image image.qcow:
$sudo /usr/local/etc/rc.d/kqemu onestart
$kvm -m 512 -usbdevice tablet -hda image.qcow

$qemu --enable-kvm /images/winXP-32-virtio.qcow2 -m 2000 -vnc :10 -usbdevice tablet -net nic,model=rtl8139 -net tap -usbdevice host:1483:c007

Options
-full-screen    start in full screen
-vnc display    start a VNC server on display
-no-frame       open SDL window without a frame and window decorations
-alt-grab       use Ctrl-Alt-Shift to grab mouse (instead of Ctrl-Alt)
-M machine      select emulated machine (-M ? for list)
-cpu cpu        select CPU (-cpu ? for list)
-smp n          set the number of CPUs to 'n' [default=1]
-cdrom file     use 'file' as IDE cdrom image (cdrom is ide1 master)
-no-fd-bootchk  disable boot signature checking for floppy disks
-no-acpi        disable ACPI
-localtime      set the real time clock to local time [default=utc]

- kqemu still works in the 0.11 branch, but is disabled by default now so
  you'll have to pass -enable-kqemu (or -kernel-kqemu as with the previous
  versions) if you want to use it.

######
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

