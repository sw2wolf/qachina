#export LDFLAGS=-L/usr/local/lib
#export CFLAGS=-I/usr/local/include
#export CPPFLAGS=-I/usr/local/include
#export GNUMAKE=gmake

#export MD=/mnt/D/qachina/db/doc
export RD=/mnt/D/RnD
export TERM=xterm
#export ERL_HOME=$HOME/erlang
#export PYTHONSTARTUP=/media/D/www/qachina/db/doc/python/python_ini.py

#if DBUS_SESSION_BUS_ADDRESS is unset dbus will set it to autolaunch,
#if it isn't set to autolaunch dbus will not be started automatically
export DBUS_SESSION_BUS_ADDRESS="foo:"

# A righteous umask
umask 22

export PATH=$PATH:$HOME/Mew/bin

alias pkg_list='lynx -dump $PKG_PATH/index.txt > pkglist.txt'
alias vi='vim'
alias ls='ls -F'
