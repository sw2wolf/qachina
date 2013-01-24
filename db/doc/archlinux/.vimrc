colorscheme torte
"Toggle Menu and Toolbar scrollbar
"set guioptions-=m
set guioptions-=T
set guioptions-=r

set nocompatible
"设置历史记录步数
set history=400 
"开启文件类型判断插件
filetype plugin on
filetype indent on
"当文件在外部被修改，自动更新该文件
set autoread
"激活鼠标的使用
"set mouse=a
set paste
set clipboard=unnamed
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

set dy=lastline

syntax enable
syntax on

"filetype on
filetype plugin on 
filetype indent on

set encoding=utf-8
"set fileencodings=utf-8,chinese
set fileencodings=utf-8,gb2312,gbk,gb18030
set termencoding=utf-8
set history=50

"set foldcolumn=2 
"set foldmethod=indent 
"set foldlevel=3

set ruler
set showcmd

au BufRead,BufNewFile *.asd set filetype=lisp

"Change work dir to current dir
autocmd BufEnter * cd %:p:h

set sessionoptions+=resize,winpos
"autocmd VIMEnter * :source ~/vim_session
"autocmd VIMLeave * :mksession! C:/session.vim

"autocmd FileType python setlocal et sta sw=4 sts=4
"autocmd FileType python setlocal foldmethod=indent

"  tab
"map <S-Down> :tabnew<CR>
"map <S-Left> :tabp<CR>
"map <S-Right> :tabn<CR>

map <S-Left> :b#<CR> :bd#<CR>

map <F12> :mksession! ~/vim_session <cr> " Quick write session with F2
map <F2> :source ~/vim_session <cr>     " And load session with F3

nmap <C-F7> :%!xxd -g 1<CR>
nmap <S-F7> :%!xxd -r<CR>

"let g:slimv_swank_cmd = '! xterm -e sbcl --load ~/.vim/slime/start-swank.lisp &'
"let g:slimv_browser_cmd = "opera"
"let g:swank_log = 1

" debug VIM
"set verbosefile=test.log
"set verbose=20
