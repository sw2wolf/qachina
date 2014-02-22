
;;;;;;
#apt-get install gcc libsdl1.2-dev zlib1g-dev libasound2-dev linux-kernel-headers pkg-config libgnutls-dev
./configure --prefix=$HOME/qemu --target-list="i386-softmmu" --audio-drv-list="alsa oss" --enable-mixemu

make
make install

sudo modprobe kvm
sudo modprobe kvm-intel  //如果你的是INTEL处理器就用这个
sudo modprobe kvm-amd  //如果你的是AMD处理器就用这个

;;;;;;
#First let’s create the bridge:
sudo ifconfig bridge0 create

#Then let’s create the tap interface:
sudo ifconfig tap0 create

#Now let’s bridge our physical interface with our tap interface:
sudo ifconfig bridge0 addm em0 addm tap0 up

#Since we will be connecting to the tap device with regular users, let’s allow regular users to connect to our tap device:
sudo sysctl net.link.tap.user_open=1 
net.link.tap.user_open: 0 -> 1 

sudo sysctl net.link.tap.up_on_open=1 
net.link.tap.up_on_open: 0 -> 1

#Also let’s fix the permissions on the file for the tap0 device:
sudo chown :elatov /dev/tap0 
sudo chmod 660 /dev/tap0

#Lastly we can use the “VDE” (Virtual Distributed Ethernet) 
cd /usr/ports/net/vde2 
/usr/ports/net/vde2> sudo make install clean

#Here is the config I used:
/usr/ports/net/vde2> make showconfig 
===> The following configuration options are available for vde2-2.3.2: 
PYTHON=on: Python bindings 
===> Use 'make config' to modify these settings

Now let’s create a VDE-Switch and add tap0 as our uplink:
elatov@freebsd:~> sudo vde_switch -d -s /tmp/vde1 -M /tmp/mgmt1 -tap tap0 -m 660 -g elatov --mgmtmode 660 --mgmtgroup elatov 

Here are all the arguments explained:
-d option tells vde_switch to run as daemon or background process. 
-s is the complete path to data socket for the switch. 
-M specifies where to create the management socket for the switch. 
-t specifies the tap interface name that connected to the switch. 
-m specifies mode of data socket -g specified group owner of data socket 
–mgmtmode specifies mode of mgmt socket 
–mgmtgroup specifies group owner of mgmt socket

Now let’s login into the virtual switch:
elatov@freebsd:~> unixterm /tmp/mgmt1 
VDE switch V.2.3.2 (C) 
Virtual Square Team (coord. R. Davoli) 2005,2006,2007 - GPLv2 

vde$ ds/showinfo 
0000 DATA END WITH '.' 
ctl dir /tmp/vde1 
std mode 0660 
. 
1000 Success 

vde$ port/allprint 
0000 DATA END WITH '.' 
Port 0001 untagged_vlan=0000 ACTIVE - Unnamed Allocatable 
Current User: NONE Access Control: (User: NONE - Group: NONE)
 -- endpoint ID 0007 module tuntap : tap0 
. 
1000 Success 

vde$ vlan/allprint 
0000 DATA END WITH '.' 
VLAN 0000 
-- Port 0001 tagged=0 active=1 status=Forwarding 
. 
1000 Success 

#Now starting up the VM and connecting it to our VDE-Switch:
$vdeqemu -hda winxp.img -m 256 -kernel-kqemu -vnc :0 -localtime -no-acpi -net vde,sock=/tmp/vde1 -net nic,model=e1000

Now checking out the switch ports:
elatov@freebsd:~>unixterm /tmp/mgmt1 
VDE switch V.2.3.2 
(C) Virtual Square Team (coord. R. Davoli) 2005,2006,2007 - GPLv2 

vde$ port/allprint 
0000 DATA END WITH '.' 
Port 0001 untagged_vlan=0000 ACTIVE - Unnamed Allocatable 
Current User: NONE Access Control: (User: NONE - Group: NONE) 
-- endpoint ID 0007 module tuntap : tap0 
Port 0002 untagged_vlan=0000 ACTIVE - Unnamed Allocatable 
Current User: elatov Access Control: (User: NONE - Group: NONE) 
-- endpoint ID 0003 module unix prog : vdeqemu 
user=elatov PID=98624 SSH=192.168.1.102 
. 
1000 Success

#We can see that now our VM is connected to the VDE-Switch. Since all the traffic is going through our tap0 interface, we can actually run tcpdump on it to see what traffic our VM is sending. Let’s ping a machine on the local subnet from the VM and see what we see on the tap0 interface. Here is the capture from the FreeBSD host as the ping is going:

sudo tcpdump -i tap0 -n host 192.168.1.110 and icmp 

tcpdump: WARNING: tap0: no IPv4 address assigned 
tcpdump: verbose output suppressed, use -v or -vv for full protocol 
decode listening on tap0, link-type EN10MB (Ethernet), capture size 65535 bytes 
17:40:01.353744 IP 192.168.1.110 > 192.168.1.102: ICMP echo request, id 12298, seq 1, length 64 
17:40:01.353957 IP 192.168.1.102 > 192.168.1.110: ICMP echo reply, id 12298, seq 1, length 64

If you didn’t guess it, the VM’s IP is 192.168.1.110. 

Automatically:
Add the following to the /etc/rc.conf file:
cloned_interfaces="tap0 bridge0" 
ifconfig_bridge0="addm em0 addm tap0 up"

Now let’s enable the sysctl options. Add the following to the /etc/sysctl.conf file:
net.link.tap.user_open=1 
net.link.tap.up_on_open=1

Next let’s setup the appropriate permissions for our tap0 device. Add the following to the /etc/devfs.conf file:
own tap0 root:elatov 
perm tap0 660

Lastly, create the VDE-Switch boot. Add the following to the /etc/rc.local file:
/usr/local/bin/vde_switch -d -s /tmp/vde1 -M /tmp/mgmt1 -tap tap0 -m 660 -g elatov --mgmtmode 660 --mgmtgroup elatov

I wanted to make sure my VDE-Switch was created:
elatov@freebsd:~> unixterm /tmp/mgmt1 
VDE switch V.2.3.2 
(C) Virtual Square Team (coord. R. Davoli) 2005,2006,2007 - GPLv2 

vde$ ds/showinfo 
0000 DATA END WITH '.' 
ctl dir /tmp/vde1 
std mode 0660 
. 
1000 Success 

vde$ port/allprint 
0000 DATA END WITH '.' 
Port 0001 untagged_vlan=0000 ACTIVE - Unnamed Allocatable 
Current User: NONE Access Control: (User: NONE - Group: NONE) 
-- endpoint ID 0007 module tuntap : tap0 
. 
1000 Success 

And lastly I wanted to make sure the kernel modules were loaded:
elatov@freebsd:~>kldstat 
Id Refs Address Size Name 
1 13 0xc0400000 e9ec64 kernel 
2 1 0xc62de000 5000 if_tap.ko 
3 1 0xc6302000 9000 if_bridge.ko 
4 1 0xc630b000 6000 bridgestp.ko 
5 1 0xc647c000 e000 fuse.ko 
6 1 0xc64ef000 8000 aio.ko 
7 1 0xc64fa000 21000 kqemu.ko

;;;;;;
#Create the RAM disk whose size is 64M. 
$ dd if=/dev/zero of=disk.img bs=4096 count=16384

#Partition the disk. 
$ /sbin/fdisk -C 16065 -H 255 -S 63 disk.img

;;;;;;
I created a startup script for the first vm 
#!/bin/sh
sudo ifconfig tap0 create
sudo ifconfig tap0 0.0.0.0 promisc up
sudo ifconfig bridge0 create
sudo ifconfig bridge0 addm em0 up
sudo ifconfig bridge0 addm tap0
sudo qemu -k de -boot c -hda test.img -m 512 -localtime -monitor stdio -usb -usbdevice tablet -net nic -net tap,ifname=tap0,script=no

and one for the second

#!/bin/sh
sudo ifconfig tap1 create
sudo /sbin/ifconfig tap1 0.0.0.0 promisc up
sudo ifconfig bridge0 addm tap1
sudo qemu -k de -boot c -hda test.img -m 512 -localtime -monitor stdio -usb -usbdevice tablet -net nic,macaddr=52:54:00:12:34:57 -net tap,ifname=tap1,script=no

;;;;;;
How to convert VirtualBox VDI image to Qemu-KVM .QCOW image
$VBoxManage clonehd "image.vdi" "image.img" --format RAW

Convert convert the raw image to a qcow image using the following command:
$qemu-img convert -f raw -O qcow2 image.img image.qcow

Install:
$qemu -m 192 -hda winxp.qcow2 -cdrom winxp.iso -boot d

Test your new image image.qcow:
$sudo /usr/local/etc/rc.d/kqemu onestart
$kvm -m 512 -usbdevice tablet -hda image.qcow

$qemu --enable-kvm /images/winXP-32-virtio.qcow2 -m 2000 -vnc :10 -usbdevice tablet -net nic,model=rtl8139 -net tap -usbdevice host:1483:c007

Options
-full-screen    start in full screen
-vnc display    start a VNC server on display(--usbdevice tablet)
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

;;;;;;
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
