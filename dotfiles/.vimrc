syntax enable
set background=dark
set tabstop=8
set shiftwidth=2
set expandtab
set smarttab
set cindent
set modelines=5
" Break hard-links - may need to break hard-links when using Mercurial optimised clones.
set backupcopy=auto,breakhardlink ",breaksymlink

au BufNewFile,BufRead *.frag,*.vert,*.fp,*.vp,*.glsl setf glsl
