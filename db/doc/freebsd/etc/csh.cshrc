# $FreeBSD: release/9.0.0/etc/csh.cshrc 50472 1999-08-27 23:37:10Z peter $
#
# System-wide .cshrc file for csh(1).
#setenv PACKAGESITE ftp://ftp.freebsd.org/pub/FreeBSD/ports/i386/packages-9-stable/Latest/
setenv PACKAGESITE ftp://ftp.freebsd.org/pub/FreeBSD/ports/i386/packages-current/Latest/

#set prompt="[%Y-%W-%D %P]%n@%~%#"
set prompt="[%P]%n@%~%#"
set nobeep
set autolist
#set autolist=ambiguous

alias ls ls -GF
#alias cl clisp -modern -q
alias update-apps 'portmaster -a --force-config'
alias show-dep 'portmaster --show-work ./ | sort'
alias  vi vim
#alias femacs emacs -Q -nw

setenv GREP_OPTIONS --color=auto
setenv EDITOR	vim
setenv PAGER	more
setenv BLOCKSIZE	K
#setenv CCACHE_DIR /usr/ccache
#setenv TZ GMT

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
