set nocompatible

fun Plugs()
  " Haskell/Glorious/GHC

  " intero-neovim looks to be pretty early.
"  Plug 'parsonsmatt/intero-neovim'
"  Plug 'neomake/neomake' " intero-neovim likes this.

"  Plugin 'dag/vim2hs'
"  Plugin 'merijn/haskellFoldIndent'
"  Plugin 'eagletmt/neco-ghc'
"  Plugin 'Shougo/neocomplcache.vim' " required by neco-ghc
"  Plugin 'eagletmt/ghcmod-vim'
"  Plugin 'eagletmt/tinytest' " required for ghcmod-vim
"  Plugin 'Shougo/vimproc' " required by ghcmod-vim

  " Colour schemes.
  Plug 'captbaritone/molokai'
"  Plug 'flazz/vim-colorschemes'

  " Org mode.
  Plug 'jceb/vim-orgmode'
  Plug 'tpope/vim-speeddating' " dependency

  Plug 'idris-hackers/idris-vim'

  " Misc.
  Plug 'kien/ctrlp.vim'
  Plug 'scrooloose/nerdtree'
  Plug 'scrooloose/nerdcommenter'
  Plug 'scrooloose/syntastic'
  Plug 'tpope/vim-surround'
  Plug 'bling/vim-airline'
  Plug 'airblade/vim-gitgutter'
  Plug 'ledger/vim-ledger'
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
  filetype plugin indent on " apparently required?
  call plug#end()
endfun

let maplocalleader = "\\"

call Plug()
colorscheme molokai
" Override ErrorMsg color as molokai has a poor one!
autocmd FileType * highlight ErrorMsg
  \ ctermbg=lightred ctermfg=black guibg=lightred guifg=black
if has("gui_running")
  colorscheme koehler
endif

syntax on
filetype on
filetype indent on

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

" Break hard-links — may need to break hard-links when using Mercurial optimised
" clones.
set backupcopy=auto,breakhardlink ",breaksymlink

"autocmd BufNewFile,BufRead *.frag,*.vert,*.fp,*.vp,*.glsl setf glsl
"autocmd BufNewFile,BufRead *.m set filetype=modula2
"autocmd BufNewFile,BufRead *.yeti setlocal filetype=yeti ts=8 sw=4 sts=4 expandtab
"autocmd BufNewFile,BufRead *.cl,*.cool set filetype=cool
"autocmd BufNewFile,BufRead *.cup set filetype=cup
"autocmd BufNewFile,BufRead *.g4 set filetype=antlr
"autocmd BufNewFile,BufRead *.purs set filetype=haskell
"autocmd BufNewFile,BufRead *.scala,*.sbt set filetype=scala

"let g:syntastic_haskell_checkers = ["ghc_mod", "hint"]

"autocmd BufWritePost *.hs,.hsc GhcModCheckAndLintAsync

autocmd FileType make setlocal noexpandtab

augroup vimrc
  " Highlight trailing whitespace.
  autocmd FileType * match ErrorMsg /\s\+$/

  " Highlight lines exceeding textwidth.
  autocmd FileType *
        \ if &textwidth |
        \    exec '2match ErrorMsg /\%>' . string(&textwidth) . 'v.\+/' |
        \ endif
augroup end

" https://csswizardry.com/2017/01/preparing-vim-for-apples-touch-bar/
inoremap jj <esc>
inoremap jk <esc>
inoremap kj <esc>
