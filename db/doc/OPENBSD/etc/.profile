
#PKG_PATH=ftp://ftp.openbsd.org/pub/OpenBSD/5.6/packages/i386/
PKG_PATH=http://ftp.jaist.ac.jp/pub/OpenBSD/5.6/packages/i386/

MD=/mnt/D/qachina/db/doc

export PKG_PATH
export MD

alias pkg_list='lynx -dump $PKG_PATH/index.txt > pkglist.txt'
alias vi='vim'
