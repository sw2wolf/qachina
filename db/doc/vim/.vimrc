set nocompatible
set nomodeline
syntax on
"设置历史记录步数
set history=400 
"开启文件类型判断插件
filetype plugin indent on
filetype indent on
"当文件在外部被修改，自动更新该文件
set autoread
set paste
"激活鼠标的使用
"set mouse=a

set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set smarttab

set nobackup
set noswapfile

set cindent 
set smartindent 

"set incsearch 
set autoindent
set hlsearch
set showmatch
set backspace=2
"set backspace=eol,start,indent

set mps+=<:>

"colo torte

set encoding=utf-8
set fileencodings=utf-8,chinese

set history=50

"set foldcolumn=2 
"set foldmethod=indent 
"set foldlevel=3

set ruler
set showcmd

"au BufRead,BufNewFile *.scala set filetype=java
au BufRead,BufNewFile *.asd set filetype=lisp
au BufRead,BufNewFile *.clj set filetype=lisp

"i表示在插入模式
imap <CTRL-S> :w<CR>
