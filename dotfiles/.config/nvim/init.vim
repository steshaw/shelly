set nocompatible

fun Plugs()
  " Colour schemes.
  Plug 'captbaritone/molokai'
  if 0
    Plug 'crusoexia/vim-monokai'
    Plug 'joshdick/onedark.vim'
    Plug 'chriskempson/base16-vim'
    Plug 'flazz/vim-colorschemes'
  endif

  " Org mode.
  Plug 'jceb/vim-orgmode'
  Plug 'tpope/vim-speeddating' " dependency

  " Modes
  Plug 'idris-hackers/idris-vim'
  Plug 'ledger/vim-ledger'
  Plug 'chr4/nginx.vim'

  " Misc.
  Plug 'ntpeters/vim-better-whitespace'
  Plug 'whatyouhide/vim-lengthmatters'
  Plug 'kien/ctrlp.vim'
  Plug 'scrooloose/nerdtree'
  Plug 'scrooloose/nerdcommenter'
  Plug 'scrooloose/syntastic'
  Plug 'tpope/vim-surround'
  Plug 'bling/vim-airline'
  Plug 'airblade/vim-gitgutter'
endfun

fun Plug()
  " Install vim-plug if we don't already have it
  if empty(glob('~/.vim/autoload/plug.vim'))
    silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
      \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
  endif
  if has('nvim')
    if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
      silent !mkdir -p ~/.local/share/nvim/site/autoload
      silent !ln -s ~/.vim/autoload/plug.vim ~/.local/share/nvim/site/autoload/plug.vim
    endif
  endif

  call plug#begin('~/.vim/plugged')
  call Plugs()
  call plug#end()
endfun

let maplocalleader = "\\"

call Plug()

let g:syntastic_sh_shellcheck_args="-x"

" Don't reset the cursor style.
" i.e. keep blinking underline from iTerm2 configuration.
set guicursor=

colorscheme molokai
" Override ErrorMsg color as molokai has a poor one!
autocmd FileType * highlight ErrorMsg
  \ ctermbg=lightred ctermfg=black guibg=lightred guifg=black
if has("gui_running")
  colorscheme koehler
endif

set showmatch
set background=dark
set tabstop=2
set shiftwidth=2
set softtabstop=2
set textwidth=80
set expandtab
set smarttab
set smartindent
set cindent
set modelines=5

if &fileformat == 'dos'
  set nofixendofline
endif

" Break hard-links — may need to break hard-links when using Mercurial optimised
" clones.
set backupcopy=auto,breakhardlink ",breaksymlink

autocmd FileType make setlocal noexpandtab

" https://csswizardry.com/2017/01/preparing-vim-for-apples-touch-bar/
inoremap jj <esc>
inoremap jk <esc>
inoremap kj <esc>

nnoremap <Leader><space> :nohlsearch<Enter>
