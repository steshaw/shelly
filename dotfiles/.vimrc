set nocompatible

fun Vundle()
  filetype off " required by Vundle
  set rtp+=~/.vim/bundle/vundle
  call vundle#rc()
  Plugin 'gmarik/vundle' " required

  "Plugin 'kien/ctrlp.vim'
  "Plugin 'scrooloose/nerdtree'
  "Plugin 'scrooloose/nerdcommenter'
  "Plugin 'scrooloose/syntastic'
  "Plugin 'tpope/vim-surround'
  "Plugin 'flazz/vim-colorschemes'
  Plugin 'bling/vim-airline'
  Plugin 'airblade/vim-gitgutter'

  Plugin 'dag/vim2hs'
  Plugin 'merijn/haskellFoldIndent'

  Plugin 'eagletmt/neco-ghc'
  Plugin 'Shougo/neocomplcache.vim' " required by neco-ghc

  Plugin 'eagletmt/ghcmod-vim'
  Plugin 'eagletmt/tinytest' " required for ghcmod-vim
  Plugin 'Shougo/vimproc' " required by ghcmod-vim

  filetype plugin indent on " required by Vundle
endf
call Vundle()

syntax enable
filetype indent on
filetype on

set showmatch
set background=dark
set tabstop=4
set shiftwidth=2
set softtabstop=2
"set textwidth=120
set expandtab
set smarttab
set smartindent
set cindent
set modelines=5

if has("gui_running")
  colorscheme koehler
endif

"execute pathogen#infect()

" Break hard-links - may need to break hard-links when using Mercurial optimised clones.
set backupcopy=auto,breakhardlink ",breaksymlink

autocmd BufNewFile,BufRead *.frag,*.vert,*.fp,*.vp,*.glsl setf glsl
autocmd BufNewFile,BufRead *.m set filetype=modula2
autocmd BufNewFile,BufRead *.yeti setlocal filetype=yeti ts=8 sw=4 sts=4 expandtab
autocmd BufNewFile,BufRead *.cl,*.cool set filetype=cool
autocmd BufNewFile,BufRead *.cup set filetype=cup
autocmd BufNewFile,BufRead *.g4 set filetype=antlr
autocmd BufNewFile,BufRead *.idr set filetype=haskell
autocmd BufNewFile,BufRead *.purs set filetype=haskell

autocmd FileType make setlocal noexpandtab
