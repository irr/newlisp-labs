set guifont=Ubuntu\ Mono\ 13
set tabstop=4
set shiftwidth=4
set softtabstop=4
set viminfo=""
set nobackup
set nowritebackup
set noswapfile
set fileformat=unix
set paste
set pastetoggle=<F5>
set guioptions-=T
set laststatus=2
set statusline=%02n:%t[%{strlen(&fenc)?&fenc:'none'},%{&ff}]%h%m%r%y%=%c,%l/%L\ %P
set showtabline=1
set expandtab
retab

" mkdir -p ~/.vim/autoload ~/.vim/bundle && \
" curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim
execute pathogen#infect()
syntax on
filetype plugin indent on

au BufRead,BufNewFile *.lsp set filetype=newlisp
au! Syntax newlisp source /opt/lisp/newlisp/util/newlisp.vim

au VimLeave * if filereadable("~/.vim//.netrwhist")|call delete(".vim/.netrwhist")|endif

vnoremap <C-X> "+x
vnoremap <S-Del> "+x

vnoremap <C-C> "+y
vnoremap <C-Insert> "+y

map <C-V> "+p
map <S-Insert> "+p

cmap <C-V> <C-R>+
cmap <S-Insert> <C-R>+

map <F2> :set fileencoding=utf-8<CR>:set fileformat=unix<CR>:w<CR>

set t_Co=256

