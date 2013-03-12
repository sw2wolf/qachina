# $FreeBSD: src/share/skel/dot.cshrc,v 1.14.6.1 2008/11/25 02:59:29 kensmith Exp $
#
# .cshrc - csh resource script, read at beginning of execution by each shell
#
# see also csh(1), environ(7).
setenv MD /media/D/qachina/db/doc
setenv GOOS freebsd
setenv GOARCH 386
setenv GOROOT /usr/local/go
#setenv EVILVTE_CONF /home/sw2wolf/evilvte-config.h
setenv LDFLAGS -L/usr/local/lib
setenv CFLAGS  -I/usr/local/include

# for gnu prolog
# setenv LOCALSZ 16384 #32768  #control stack (environments and choice-points)
# setenv GLOBALSZ 16384 #32768 #heap (compound terms)
# setenv TRAILSZ 16384  #conditional bindings (bindings to undo at backtracking)
# setenv CSTRSZ  16384  #finite domain constraint stack (FD variables and constraints
# setenv MAX_ATOM  16384 #32768 #atom table

# A righteous umask
umask 22

set path = (. /sbin /bin /usr/sbin /usr/bin /usr/games /usr/local/sbin /usr/local/bin  $HOME/bin $HOME/ecl/bin $HOME/.cabal/bin $HOME/yap/bin $HOME/swi-prolog/bin $HOME/ocaml/bin $HOME/opam/bin)
#setenv PYTHONSTARTUP /media/D/www/qachina/db/doc/python/python_ini.py
