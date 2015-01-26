
;;;;;
virtual floppy, like -fda vvfat:directory
ftp 10.0.2.2 -u ***

如果你遇到鼠标始终挂在右下角,请在启动QEMU前运行
export SDL_VIDEO_X11_DGAMOUSE=0

;;;;;
$qemu-system-x86_64 -net nic,model=?
qemu: Supported NIC models: ne2k_pci,i82551,i82557b,i82559er,rtl8139,e1000,pcnet,virtio

在QEMU monitor中查看网络的信息如下：
(qemu) info network

 为了使虚拟机能够与外界通信，Qemu需要为虚拟机提供网络设备。Qemu支持的常用网卡包括NE2000、rtl8139、pcnet32等。命令行上用-net nic为虚拟机创建虚拟机网卡。例如，qemu的命令行选项
                            -net nic,model=pcnet
表示为虚拟机添加一块pcnet型的以太网卡。如果省略model参数则qemu会默认选择一种网卡类型，目前采用的是Intel 82540EM（手册里说明的是e1000），可以在虚拟机启动后执行lspci命令查看。有了虚拟网络设备，下面的问题是如何用这些设备来联网。

首先，虚拟机的网络设备连接在qemu虚拟的VLAN中。每个qemu的运行实例是宿主机中的一个进程，而每个这样的进程中可以虚拟一些VLAN，虚拟机网络设备接入这些VLAN中。当某个VLAN上连接的网络设备发送数据帧，与它在同一个VLAN中的其它网路设备都能接收到数据帧。上面的例子中对虚拟机的pcnet网卡没有指定其连接的VLAN号，那么qemu默认会将该网卡连入vlan0。下面这个例子更具一般性：
      -net nic,model=pcnet -net nic,model=rtl8139,vlan=1, -net nic,model=ne2k_pci,vlan=1
该命令为虚拟机创建了三块网卡，其中第一块网卡类型是pcnet，连入vlan0；第二块网卡类型是 rtl8139，第三块网卡类型是ne2k_pci，这两块都连入vlan1，所以第二块网卡与第三块网卡可以互相通信，但它们与第一块网卡不能直接通信。

接下来，各个VLAN再通过qemu提供的4种通信方式与外界联网。

    User mode stack：这种方式在qemu进程中实现一个协议栈，负责在虚拟机VLAN和外部网络之间转发数据。可以将该协议栈视为虚拟机与外部网络之间的一个NAT服务器，外部网络不能主动与虚拟机通信。虚拟机VLAN中的各个网络接口只能置于10.0.2.0子网中，所以这种方式只能与外部网络进行有限的通信。此外，可以用-redir选项为宿主机和虚拟机的两个TCP或UDP端口建立映射，实现宿主机和虚拟机在特殊要求下的通信（例如X-server或ssh）。User mode stack通信方式由-net user选项启用，如果不显式指定通信方式，则这种方式是qemu默认的通信方式。

    socket：这种方式又分为TCP和UDP两种类型。
    （1）TCP：为一个VLAN创建一个套接字，让该套接字在指定的TCP端口上监听，而其他VLAN连接到该套接字上，从而将多个VLAN连接起来。缺点在于如果监听套接字所在qemu进程崩溃，整个连接就无法工作。监听套接字所在VLAN通过-net socket,listen选项启用，其他VLAN通过-net socket,connect选项启用。
    （2）UDP：所有VLAN连接到一个多播套接字上，从而使多个VLAN通过一个总线通信。所有VLAN都通过-net socket,mcast选项启用。

    TAP：这种方式首先需要在宿主机中创建并配置一个TAP设备，qemu进程将该TAP设备连接到虚拟机VLAN中。其次，为了实现虚拟机与外部网络的通信，在宿主机中通常还要创建并配置一个网桥，并将宿主机的网络接口（通常是eth0）作为该网桥的一个接口。最后，只要将TAP设备作为网桥的另一个接口，虚拟机VLAN通过TAP设备就可以与外部网络完全通信了。这是因为，宿主机的eth0接口作为网桥的接口，与外部网络连接；TAP设备作为网桥的另一个接口，与虚拟机VLAN连接，这样两个网络就连通了。此时，网桥在这两个网络之间转发数据帧。
    这里有两个问题需要注意：
    （1）网桥的转发工作需要得到内核的支持，所以在编译宿主机内核时需要选择与桥接相关的配置选项。
    （2）当宿主机eth0接口作为网桥接口时，不能为其配置IP地址，而要位将IP地址配置给网桥。
    TAP方式由-net tap选项启用。
    VDE：这种方式首先要启动一个VDE进程，该进程打开一个TAP设备，然后各个虚拟机VLAN与VDE进程连接，这样各个VLAN就可以通过TAP设备连接起来。VDE进程通过执行vde_switch命令启动，各个VLAN所在qemu进程通过执行veqe命令启动，这些VLAN就可以与VDE进程连接了。

以上四种通信方式中，socket方式和VDE方式用于虚拟机VLAN之间的连接，而user mode stack方式与外部网路的通信比较有限，所以下面主要讨论TAP方式的配置。

在没有做配置之前，首先需要对TAP设备有所认识。TUN/TAP是内核支持的网络虚拟设备，这种网络设备完全由的软件实现。与网络硬件设备不同，TUN/TAP负责在内核协议栈与用户进程之间传送协议数据单元。TUN与TAP的区别在于，TUN工作在网络层，而TAP则工作在数据链路层。具体在运行TCP/IP的以太网中，TUN与应用程序交换IP包，而TAP与应用程序交换以太帧。所以TUN通常涉及路由，而TAP则常用于网络桥接。TUN/TAP的典型应用包括：OpenVPN、OpenSSH 以及虚拟机网络。

;;;;;
./configure --prefix=$HOME/qemu --target-list="i386-softmmu i386-bsd-user" --audio-drv-list="alsa oss" --enable-mixemu

;;;;;;
typical usage of OS under qemu in snapshot mode [to commit made changes hit left_CTRL+left_ALT+2 and type commit]:
% qemu -hda ~/qemu/win2000.img -m 256 -localtime -snapshot

1. to switch focus between qemu mouse focus and X11 mouse focus use: left_CTRL+left_ALT

2. to ensure that You use kqemu kernel module in user mode hit left_CTRL+left_ALT+2 and type info kqemu, if everything is ok You will see:
Code: Select all
kqemu support: enabled for user code

if not You will see:
kqemu support: disabled

if disabled it will work, but terribly slow, You will be running at Pentium 75 speed at host with CPU AthlonXP 1.66GHz.

3. to switch between qemu console and qemu os emualtion use left_CTRL+left_ALT+2 to go to console and left_CTRL+left_ALT+1 to back to emulation.

4. enabling network on emulated os, use DHCP configuration inside emulated os to get automatic IP adress 10.x.x.x

5. with -soundhw sb16 or -soundhw es1370 to emulate Sound Blaster 16, or Sound Blaster 128. You can also enable standart annoying BEEP with -soundhw pcspk. You can also enable all three of them like that: -soundhw sb16,es1370,pcspk, or just BEEP + one of them: -soundhw es1370,pcspk

;;;;;;
考虑是否加载 Kqemu

这是一个内核模块，能够提高Qemu的效率，但是，放到内核中的问题是，会影响整个系统的稳定性，而当前代码的稳定性似乎与内核还有差距，我在用Qemu来运行一些代码的时候遇到过因为Kqemu而把整个系统崩溃的情况，所以，慎重考虑是否应该加载这个内核模块 
操作虚拟机的硬盘

使用 mdconfig(8) 把硬盘文件映射成 /dev/mdx 然后 通过mount /dev/mdxsy 来直接读写虚拟机的硬盘，不需要启动虚拟机，速度快。

;;;;;;
#/usr/local/etc/rc.d/kqemu onestart

$qemu -hda winxp.qcow2 -m 192 -kernel-kqemu -enable-kqemu -localtime -net nic -net user -nographic -daemonize -redir tcp:3389::3389 -localtime

$rdesktop -u *** -p 123 -g 1440x880 -D -K localhost:3389

;;;;;;
# kldload kqemu
# kldload aio
# cd /media/qemu
# qemu-img create -f qcow2 winxp.img 40G
# qemu -cdrom WindowsXP.iso -hda winxp.img -m 1024 -boot d -kernel-kqemu -localtime

为了确保 kqemu 模块可用，按住左边的 Ctrl + Alt + 2，键入 info kqemu，可见 kquemu support: enabled for user and kernel code。再按住左边的 Ctrl + Alt + 1 可退出。

# qemu -hda winxp.img -m 1024 -kernel-kqemu -soundhw ac97 -localtime

FreeBSD 里的设置，可参考 https://wiki.freebsd.org/qemu。包括，

在 /etc/rc.conf 里添加
devfs_system_ruleset="localrules"
kqemu_enable="YES"

在 /boot/loader.conf 里添加
if_bridge_load="YES"
if_tap_load="YES"
aio_load="YES"
kqemu_load="YES"

在 /etc/sysctl.conf 里添加
net.link.tap.up_on_open=1
net.link.tap.user_open=1

;;;;;;
Monitor
# 有時候會遇到 vnc 無法開啟的情況，使用 screendump 輸出螢幕內容。
# 再用 http://www.online-convert.com 轉成 jpeg 觀看。
(qemu) screendump screenshot.ppm
(qemu) info jit
Translation buffer state:
gen code size       3785504/4089856
TB count            11244/32768
TB avg target size  14 max=595 bytes
TB avg host size    336 bytes (expansion ratio: 23.4)
cross page TB count 25 (0%) // guest TB 對映的 guest binary 跨 guest page 的個數和比例。
direct jump count   7569 (67%) (2 jumps=5173 46%)

Statistics:
TB flush count      0
TB invalidate count 2737
TLB flush count     13869
# 顯示內存、MMIO、IO 地址分佈。
# 以縮排顯示階層關係。
(qemu) info mtree
I/O
0000000000000000-000000000000ffff (prio 0): io
  0000000000000020-0000000000000021 (prio 0): pic

使用 Ctrl + Alt + 2 组合键切换到 QEMU 终端 (QEMU Monitor)，然后输入 gdbserver ，启动 gdbserver 服务。这时启动另外一个终端窗口，输入 gdb vmlinux 命令进行调试：
(gdb) target remote localhost:1234               /*使用 gdbserver 进行调试命令*/

或是將 QEMU 終端導至標準輸出。
$ qemu -hda linux-0.2.img -vnc 0.0.0.0:1 -monitor stdio

Snapshot
先確定裝置格式支援 snapshot。raw 不支援 snapshot。Monitor。 
(qemu) info block
ide0-hd0: removable=0 io-status=ok file=linux-0.2.img ro=0 drv=raw encrypted=0
ide1-cd0: removable=1 locked=0 tray-open=0 io-status=ok [not inserted]
floppy0: removable=1 locked=0 tray-open=0 [not inserted]
sd0: removable=1 locked=0 tray-open=0 [not inserted]
(qemu) savevm
Device 'ide0-hd0' is writable but does not support snapshots.

用 qemu-img 轉換硬盤映像成 qcow2 格式。 
$ qemu-img convert -O qcow2 linux-0.2.img linux-0.2.qcow2

在 monitor 在下 savevm 把 RAM、device state 和 disk 存到當前使用的硬盤映像11)。 
(qemu) savevm
(qemu) loadvm
(qemu) info snapshots

;;;;;;
# SET THIS TO YOUR MAIN INTERFACE
iface=em0

echo "Make configuration persistent through reboots:"
echo net.link.tap.user_open=1 >> /etc/sysctl.conf
echo net.link.tap.up_on_open=1 >> /etc/sysctl.conf
echo chmod 0660 /dev/tap0 >> /etc/rc.local # this should be done with devfs(8)
echo 'cloned_interfaces="tap0 bridge0"' >> /etc/rc.conf
echo 'ifconfig_bridge0="addm '$iface' addm tap0 up"' >> /etc/rc.conf
echo 'Actually configure everything (unless you want to reboot at this point):'
/etc/rc.d/sysctl start
ifconfig bridge0 create
ifconfig tap0 create
ifconfig bridge0 addm $iface addm tap0 up
chmod 0660 /dev/tap0

# Run the instance
qemu-system-x86_64 -net nic,model=e1000 -net tap,name=tap0,script=no -cdrom 8.0-BETA3-amd64-bootonly.iso

