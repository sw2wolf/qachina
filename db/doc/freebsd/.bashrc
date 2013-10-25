export MD=/media/D/qachina/db/doc
export LDFLAGS=-L/usr/local/lib
export CFLAGS=-I/usr/local/include
export CPPFLAGS=-I/usr/local/include
export GNUMAKE=gnumake

#export PYTHONSTARTUP=/media/D/www/qachina/db/doc/python/python_ini.py

# A righteous umask
umask 22

CAML_LD_LIBRARY_PATH=/home/sw2wolf/.opam/system/lib/stublibs:/home/sw2wolf/ocaml//lib/ocaml/stublibs; export CAML_LD_LIBRARY_PATH;
OCAML_TOPLEVEL_PATH=/home/sw2wolf/.opam/system/lib/toplevel; export OCAML_TOPLEVEL_PATH;
MANPATH=/home/sw2wolf/.opam/system/man:; export MANPATH;
PATH=/home/sw2wolf/.opam/system/bin:.:/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/local/bin:/home/sw2wolf/bin:/home/sw2wolf/clisp/bin:/home/sw2wolf/ocaml/bin:/home/sw2wolf/.cabal/bin:/home/sw2wolf/opam/bin/; export PATH;
