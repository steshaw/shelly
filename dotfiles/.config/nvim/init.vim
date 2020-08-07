set nocompatible

fun! Plugs()
  "
  " Colour schemes.
  "
  " Collections.
  Plug 'flazz/vim-colorschemes'
  Plug 'mswift42/vim-themes'
  " Specific themes.
  Plug 'captbaritone/molokai'
  Plug 'dracula/vim', { 'as': 'dracula' }
  Plug 'joshdick/onedark.vim'
  Plug 'lifepillar/vim-solarized8'
  Plug 'morhetz/gruvbox'
  if 1
    Plug 'crusoexia/vim-monokai'
  else
    Plug 'sickill/vim-monokai'
  endif

  " Org mode.
  Plug 'jceb/vim-orgmode'
  Plug 'tpope/vim-speeddating' " Required dependency.
  " SyntaxRange allows timedot in src blocks.
  " See .config/nvim/after/syntax/org.vim
  Plug 'vim-scripts/SyntaxRange'

  " Ledger.
  Plug 'ledger/vim-ledger'
  Plug '~/Code/steshaw/timedot-vim'

  "
  " vim-lsp.
  " https://github.com/prabirshrestha/vim-lsp
  "
"  Plug 'prabirshrestha/async.vim'
"  Plug 'prabirshrestha/vim-lsp'

  " Modes
  Plug 'LnL7/vim-nix'
  Plug 'cespare/vim-toml'
  Plug 'chr4/nginx.vim'
  Plug 'dense-analysis/ale'
  Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
  Plug 'hashivim/vim-terraform'
  Plug 'idris-hackers/idris-vim'
  Plug 'nelsyeung/twig.vim'
  Plug 'ollykel/v-vim'
  Plug 'purescript-contrib/purescript-vim'
  Plug 'rust-lang/rust.vim'
  Plug 'scrooloose/syntastic'
  "Plug 'sheerun/vim-polyglot'
  Plug 'vmchale/dhall-vim'
  Plug 'vmchale/ipkg-vim'
  Plug 'ziglang/zig.vim'

  " TypeScript
  Plug 'Quramy/tsuquyomi'
  Plug 'leafgarland/typescript-vim'

  " Haskell Formatting
  Plug 'sdiehl/vim-ormolu'

  "
  " Miscellaneous.
  "
  Plug 'Shougo/deol.nvim'
  Plug 'airblade/vim-gitgutter'
  Plug 'ctrlpvim/ctrlp.vim'
  Plug 'editorconfig/editorconfig-vim'
  Plug 'ntpeters/vim-better-whitespace'
  Plug 'rhlobo/vim-super-retab'
  Plug 'scrooloose/nerdcommenter'
  Plug 'scrooloose/nerdtree'
  Plug 'tpope/vim-surround'
  Plug 'tpope/vim-repeat'
  Plug 'vim-airline/vim-airline'
  Plug 'vim-airline/vim-airline-themes'
  Plug 'whatyouhide/vim-lengthmatters'

  "Plug 'qualiabyte/vim-colorstepper'
  Plug 'xolox/vim-misc'
  Plug 'xolox/vim-colorscheme-switcher'

  " Turn off search highlighting when done.
  Plug 'romainl/vim-cool'

  Plug 'neoclide/coc.nvim', {'branch': 'release'}

  "
  " neosnippet
  "
  " See https://github.com/Shougo/neosnippet.vim#vim-plug
  "
  Plug 'Shougo/neosnippet.vim'
  Plug 'Shougo/neosnippet-snippets'
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

" Ormolu
let g:ormolu_disable=1
nnoremap tf :call RunOrmolu()<CR>

"let mapleader="\<Space>"
let maplocalleader = ','

call Plug()

let g:colorscheme_switcher_keep_background = 1

" ctrlp
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']

" syntastic
let g:syntastic_sh_shellcheck_args = "-x"

" vim-airline
let g:airline_powerline_fonts = 1

" ALE
let g:airline#extensions#ale#enabled = 1
nmap <silent> <leader>aj <Plug>(ale_next_wrap)
nmap <silent> <leader>ak <Plug>(ale_previous_wrap)
nmap <silent> <C-j> <Plug>(ale_next_wrap)
nmap <silent> <C-k> <Plug>(ale_previous_wrap)

" ==========================================================================
" Settings
" ==========================================================================

" Don't reset the cursor style.
" i.e. keep blinking underline from terminal configuration.
set guicursor=

set background=dark
set cindent
set cursorline
set encoding=utf-8
set expandtab
set hlsearch! " Hightlight searchs.
set modelines=5
set number
set shiftwidth=2
set showmatch
set smartindent
set smarttab
set softtabstop=2
set tabstop=20
set termguicolors
set textwidth=76

if &fileformat == 'dos'
  set nofixendofline
endif

" Break hard-links — may need to break hard-links when using Mercurial optimised
" clones.
set backupcopy=auto,breakhardlink ",breaksymlink

" ==========================================================================
" Color Theme
" ==========================================================================
" Set preferred theme if possible.
try
"  colorscheme molokai
  " FIXME: Change the color of the Coc floating error and warning boxes.
  " Override ErrorMsg color as molokai has a poor one!
"  autocmd FileType * highlight ErrorMsg
"    \ ctermbg=lightred ctermfg=black guibg=lightred guifg=black

"  colorscheme onedark
"  colorscheme dracula
"  colorscheme gruvbox "
"  colorscheme monokai
"  colorscheme solarized8_high

"  colorscheme pt_black
"  colorscheme SerialExperimentsLain
"  colorscheme 'space-vim-dark'
"  colorscheme stereokai
  colorscheme Tomorrow-Night
catch
  " Fallback theme.
  silent! colorscheme darkblue
endtry

" ==========================================================================

autocmd FileType make setlocal noexpandtab | set tabstop=2
autocmd FileType markdown set spell
autocmd FileType org set spell " FIXME: Doesn't work for org mode.
"autocmd BufNewFile,BufRead *.v set filetype=go
autocmd BufNewFile,BufRead .mrconfig set filetype=toml
autocmd BufNewFile,BufRead .Brewfile set filetype=ruby

nnoremap <Leader><space> :nohlsearch<Enter>

" ==========================================================================
" Org mode configuration.
" ==========================================================================
let g:org_agenda_files=['~/dev/tlcsrc/log/log.org']
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

" ==========================================================================

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

" ==========================================================================
" Andres Löh's COC setup
" COC / ghcide keybindings and functionality
" ==========================================================================

" Use `[c` and `]c` for navigate diagnostics
nmap <silent> [c <Plug>(coc-diagnostic-prev)
nmap <silent> ]c <Plug>(coc-diagnostic-next)

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

xmap <leader>a <Plug>(coc-codeaction-selected)
nmap <leader>a <Plug>(coc-codeaction-selected)

" Remap for do codeAction of current line
nmap <leader>ac <Plug>(coc-codeaction)
" Fix autofix problem of current line
nmap <leader>qf <Plug>(coc-fix-current)

" Remap for do action format
nnoremap <leader>F :call CocAction('format')<CR>
"
" Remap for format selected region
nmap <leader>f  <Plug>(coc-format-selected)

" Use K for show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if &filetype == 'vim'
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')

" Remap for rename current word
nmap <leader>rn <Plug>(coc-rename)

" Show all diagnostics
nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" Show commands
nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols
nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list
nnoremap <silent> <space>p  :<C-u>CocListResume<CR>

" Terminal mode allow escape
tnoremap <Esc> <C-\><C-n>
