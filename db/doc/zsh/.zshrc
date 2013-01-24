#PS1='%1d@%M(%*)(%d)%#'
PS1='(%*)(%d)%#'

#历史纪录条目数量
export HISTSIZE=1000
#注销后保存的历史纪录条目数量
export SAVEHIST=10000
#历史纪录文件
export HISTFILE=~/.zsh_history

#以附加的方式写入历史纪录
setopt INC_APPEND_HISTORY
#如果连续输入的命令相同，历史纪录中只保留一个
setopt HIST_IGNORE_DUPS     
#为历史纪录中的命令添加时间戳     
setopt EXTENDED_HISTORY 

#禁用 core dumps
limit coredumpsize 0

# emacs
bindkey -e
#设置 [DEL]键 为删除当前字符
bindkey "\e[3~" delete-char

# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/sw2wolf/.zshrc'

#自动补全功能
setopt AUTO_LIST
setopt AUTO_MENU
setopt MENU_COMPLETE

autoload -Uz compinit
compinit

# Completion caching
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path .zcache

# End of lines added by compinstall
#
#autoload -U zsh-mime-setup
#zsh-mime-setup
#alias -s png=pho
export PATH=$PATH:/usr/local/bin:~/bin:~/.cabal/bin:/opt/java/jre/bin
alias ls='ls -F --color=auto'
alias grep='grep --color=auto'
#alias ruby9="~/ruby-1.9.1/bin/ruby"
#alias irb9="~/ruby-1.9.1/bin/irb"
#alias gem9="~/ruby-1.9.1/bin/gem"

#路径别名
##进入相应的路径时只要 cd ~xxx
hash -d WWW="/media/F/www"

#zmodload zsh/mathfunc
#function calc { echo $(($@)) }
