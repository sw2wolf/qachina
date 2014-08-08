#export LDFLAGS=-L/usr/local/lib
#export CFLAGS=-I/usr/local/include
#export CPPFLAGS=-I/usr/local/include
#export GNUMAKE=gmake

export MD=/media/D/qachina/db/doc
#export ERL_HOME=$HOME/erlang
#export PYTHONSTARTUP=/media/D/www/qachina/db/doc/python/python_ini.py

#if DBUS_SESSION_BUS_ADDRESS is unset dbus will set it to autolaunch,
#if it isn't set to autolaunch dbus will not be started automatically
export DBUS_SESSION_BUS_ADDRESS="foo:"

# A righteous umask
umask 22

PATH=".:/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/local/bin:$HOME/bin:$HOME/dwb/bin:$HOME/ccl:$HOME/maxima/bin"
export PATH
