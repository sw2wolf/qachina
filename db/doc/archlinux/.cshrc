# $FreeBSD: src/share/skel/dot.cshrc,v 1.14.6.1 2008/11/25 02:59:29 kensmith Exp $
#
# .cshrc - csh resource script, read at beginning of execution by each shell
#
# see also csh(1), environ(7).


#set prompt="[%Y-%W-%D %P]%n@%~%#"
set prompt="[%P]%n@%~%#"
set nobeep
set autolist

alias ls ls -F
#alias cl clisp -modern -q
#set autolist=ambiguous

setenv PACKAGESITE ftp://ftp.cn.freebsd.org/pub/FreeBSD/ports/i386/packages-8.2-release/All/

setenv CCL_DEFAULT_DIRECTORY  /media/E/RnD/clisp/ccl-1.7/
# A righteous umask
umask 22

set path = (/sbin /bin /usr/sbin /usr/bin /usr/games /usr/local/sbin /usr/local/bin $HOME/bin)
alias  vi vim
alias femacs emacs -Q -nw
setenv	EDITOR	vim
setenv	PAGER	more
setenv	BLOCKSIZE	K
setenv  LANG zh_CN.UTF-8
setenv  LC_LANG zh_CN.UTF-8
setenv  LC_TYPE zh_CN.UTF-8
setenv XMODIFIERS '@im=fcitx'
#setenv PYTHONSTARTUP /media/G/www/qachina/db/doc/python/python_ini.py
setenv CCACHE_DIR /usr/ccache

bindkey "\e[3~" delete-char
bindkey "\e[1~" beginning-of-line

if ($?prompt) then
	# An interactive shell -- set some stuff up
	set filec
	set history = 100
	set savehist = 100
	set mail = (/var/mail/$USER)
	if ( $?tcsh ) then
		bindkey "^W" backward-delete-word
		bindkey -k up history-search-backward
		bindkey -k down history-search-forward
	endif
endif
