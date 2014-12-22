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

