alias ls="ls -F"
alias vi="vim"

if [ -f /etc/profile ]; then
   source /etc/profile
fi

if [ -f ~/.bashrc ]; then
   source ~/.bashrc
fi

#~/bin/wmi.lisp
#xinit &
startx
