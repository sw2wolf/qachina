export LDFLAGS=-L/usr/local/lib
export CFLAGS=-I/usr/local/include
export CPPFLAGS=-I/usr/local/include
export GNUMAKE=gmake

export MD=/media/D/qachina/db/doc
export ERL_HOME=/home/sw2wolf/erlang
#export PYTHONSTARTUP=/media/D/www/qachina/db/doc/python/python_ini.py

# A righteous umask
umask 22

CAML_LD_LIBRARY_PATH="/home/sw2wolf/.opam/system/lib/stublibs:/usr/local/lib/ocaml/stublibs"; export CAML_LD_LIBRARY_PATH;
PERL5LIB="/home/sw2wolf/.opam/system/lib/perl5"; export PERL5LIB;
OCAML_TOPLEVEL_PATH="/home/sw2wolf/.opam/system/lib/toplevel"; export OCAML_TOPLEVEL_PATH;
MANPATH="/home/sw2wolf/.opam/system/man:"; export MANPATH;
PATH="/home/sw2wolf/.opam/system/bin:.:/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/local/bin:/home/sw2wolf/bin:/home/sw2wolf/clisp/bin:/home/sw2wolf/erlang/bin:/home/sw2wolf/ccl:/home/sw2wolf/maxima/bin:/home/sw2wolf/.cabal/bin:/home/sw2wolf/tcc/bin:/home/sw2wolf/ocaml/bin:/home/sw2wolf/.opam/4.00.1/bin"; export PATH;
