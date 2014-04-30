#!/bin/sh
A=(
./configure
--with-ffcall --with-libffcall-prefix=/usr
--with-readline # --with-libreadline-prefix=/usr
--with-sigsegv # --with-libsigsegv-prefix=/usr
--with-module=bindings/glibc
--with-module=asdf
--with-module=clx/new-clx
--with-module=dbus
--with-module=readline
--with-module=regexp
--with-module=rawsock
--with-module=syscalls
--with-module=i18n
--with-module=zlib
--cbc build-dir
--with-threads=POSIX_THREADS
--prefix=/home/sw2wolf/clisp
)
$A
