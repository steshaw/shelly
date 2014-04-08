set nocompatible
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

execute pathogen#infect()

" Break hard-links - may need to break hard-links when using Mercurial optimised clones.
set backupcopy=auto,breakhardlink ",breaksymlink

au BufNewFile,BufRead *.frag,*.vert,*.fp,*.vp,*.glsl setf glsl
au BufNewFile,BufRead *.m set filetype=modula2
au BufNewFile,BufRead *.yeti setlocal filetype=yeti ts=8 sw=4 sts=4 expandtab
au BufNewFile,BufRead *.cl,*.cool set filetype=cool
au BufNewFile,BufRead *.cup set filetype=cup
au BufNewFile,BufRead *.g4 set filetype=antlr
au BufNewFile,BufRead *.idr set filetype=haskell
au BufNewFile,BufRead *.purs set filetype=haskell
