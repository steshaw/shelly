set nocompatible

fun! Plugs()
  " Colour schemes.
  Plug 'captbaritone/molokai'
  if 0
    Plug 'chriskempson/base16-vim'
    Plug 'crusoexia/vim-monokai'
    Plug 'flazz/vim-colorschemes'
    Plug 'joshdick/onedark.vim'
  endif

  " Org mode.
  Plug 'jceb/vim-orgmode'
  Plug 'tpope/vim-speeddating' " Required dependency.
  " SyntaxRange allows timedot in src blocks.
  " See .config/nvim/after/syntax/org.vim
  Plug 'vim-scripts/SyntaxRange'

  " Ledger.
  Plug 'ledger/vim-ledger'
  Plug '~/Code/steshaw/vim-timedot'

  " Modes
  Plug 'LnL7/vim-nix'
  Plug 'chr4/nginx.vim'
  Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
  Plug 'idris-hackers/idris-vim'
  Plug 'nelsyeung/twig.vim'
  Plug 'ollykel/v-vim'
  Plug 'purescript-contrib/purescript-vim'
  Plug 'rust-lang/rust.vim'
  Plug 'scrooloose/syntastic'
  Plug 'vmchale/dhall-vim'
  Plug 'vmchale/ipkg-vim'
  Plug 'dense-analysis/ale'
  Plug 'ziglang/zig.vim'

  " TypeScript
  Plug 'Quramy/tsuquyomi'
  Plug 'leafgarland/typescript-vim'

  "
  " Miscellaneous.
  "
  Plug 'Shougo/deol.nvim'
  Plug 'airblade/vim-gitgutter'
  Plug 'editorconfig/editorconfig-vim'
  Plug 'kien/ctrlp.vim'
  Plug 'ntpeters/vim-better-whitespace'
  Plug 'rhlobo/vim-super-retab'
  Plug 'scrooloose/nerdcommenter'
  Plug 'scrooloose/nerdtree'
  Plug 'tpope/vim-surround'
  Plug 'vim-airline/vim-airline'
  Plug 'vim-airline/vim-airline-themes'
  Plug 'whatyouhide/vim-lengthmatters'

  " Turn off search highlighting when done.
  Plug 'romainl/vim-cool'

  Plug 'neoclide/coc.nvim', {'branch': 'release'}

  "
  " Install neosnippet {
  "
  " See https://github.com/Shougo/neosnippet.vim#vim-plug
  "
  if has('nvim')
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
  else
    Plug 'Shougo/deoplete.nvim'
    Plug 'roxma/nvim-yarp'
    Plug 'roxma/vim-hug-neovim-rpc'
  endif
  let g:deoplete#enable_at_startup = 1

  Plug 'Shougo/neosnippet.vim'
  Plug 'Shougo/neosnippet-snippets'
  " }
endfun

" Ensure plug is installed.
fun! Plug()
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

"let mapleader="\<Space>"
let maplocalleader = ','

call Plug()

let g:syntastic_sh_shellcheck_args = "-x"

" vim-airline
let g:airline_powerline_fonts = 1

" ALE
let g:airline#extensions#ale#enabled = 1
nmap <silent> <leader>aj <Plug>(ale_next_wrap)
nmap <silent> <leader>ak <Plug>(ale_previous_wrap)
nmap <silent> <C-j> <Plug>(ale_next_wrap)
nmap <silent> <C-k> <Plug>(ale_previous_wrap)

" Don't reset the cursor style.
" i.e. keep blinking underline from iTerm2 configuration.
set guicursor=

" Set preferred theme if possible.
try
  colorscheme molokai
  " Override ErrorMsg color as molokai has a poor one!
  autocmd FileType * highlight ErrorMsg
    \ ctermbg=lightred ctermfg=black guibg=lightred guifg=black
  if has("gui_running")
    colorscheme koehler
  endif
catch
  " Fallback theme.
  silent! colorscheme elflord
endtry

set showmatch
set background=dark
set tabstop=20
set shiftwidth=2
set softtabstop=2
set textwidth=76
set expandtab
set smarttab
set smartindent
set cindent
set modelines=5
set hlsearch! " Hightlight searchs.

if &fileformat == 'dos'
  set nofixendofline
endif

" Break hard-links — may need to break hard-links when using Mercurial optimised
" clones.
set backupcopy=auto,breakhardlink ",breaksymlink

autocmd FileType make setlocal noexpandtab | set tabstop=2
"autocmd BufNewFile,BufRead *.v set filetype=go

" https://csswizardry.com/2017/01/preparing-vim-for-apples-touch-bar/
inoremap jj <esc>
inoremap kk <esc>
inoremap jk <esc>
inoremap kj <esc>

nnoremap <Leader><space> :nohlsearch<Enter>

let g:org_agenda_files=['~/dev/tlcsrc/log/log.org']

"
" Org mode configuration.
"
let g:org_heading_shade_leading_stars = 0
let g:org_indent = 1
let g:org_todo_keywords = ['TODO', 'BLOCKED', 'DOING', '|', 'DONE']
let g:org_todo_keyword_faces =
      \[
      \   ['TODO',
      \     [':foreground Yellow'
      \     ]
      \   ],
      \   ['BLOCKED',
      \     [':foreground Red'
      \     ,':weight bold'
      \     ,':slant italic'
      \     ,':decoration reverse,NONE'
      \     ]
      \   ],
      \   ['DOING',
      \     [':foreground Yellow'
      \     ,':weight bold'
      \     ,':slant italic'
      \     ]
      \   ],
      \   ['DONE',
      \     [':foreground Green'
      \     ]
      \   ]
      \ ]

" Switching windows
tnoremap <A-h> <C-\><C-N><C-w>h
tnoremap <A-j> <C-\><C-N><C-w>j
tnoremap <A-k> <C-\><C-N><C-w>k
tnoremap <A-l> <C-\><C-N><C-w>l
inoremap <A-h> <C-\><C-N><C-w>h
inoremap <A-j> <C-\><C-N><C-w>j
inoremap <A-k> <C-\><C-N><C-w>k
inoremap <A-l> <C-\><C-N><C-w>l
nnoremap <A-h> <C-w>h
nnoremap <A-j> <C-w>j
nnoremap <A-k> <C-w>k
nnoremap <A-l> <C-w>l

" ======================================================================
" COC
" ======================================================================

" Remap for format selected region
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

" Remap for do action format
nnoremap <leader> F :call CocAction('format')<CR>

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if &filetype == 'vim'
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction
