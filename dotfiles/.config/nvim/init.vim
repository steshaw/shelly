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
  Plug 'LnL7/vim-nix'
  Plug 'chr4/nginx.vim'
  Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
  Plug 'idris-hackers/idris-vim'
  Plug 'ledger/vim-ledger'
  Plug 'nelsyeung/twig.vim'
  Plug 'rust-lang/rust.vim'
  Plug 'scrooloose/syntastic'
  Plug 'vmchale/dhall-vim'
  Plug 'vmchale/ipkg-vim'
  Plug 'w0rp/ale'
  Plug 'ziglang/zig.vim'
  Plug 'purescript-contrib/purescript-vim'
  Plug 'ollykel/v-vim'

  " TypeScript
  Plug 'Quramy/tsuquyomi'
  Plug 'leafgarland/typescript-vim'

  " Misc.
  Plug 'airblade/vim-gitgutter'
  Plug 'editorconfig/editorconfig-vim'
  Plug 'kien/ctrlp.vim'
  Plug 'ntpeters/vim-better-whitespace'
  Plug 'scrooloose/nerdcommenter'
  Plug 'scrooloose/nerdtree'
  Plug 'tpope/vim-surround'
  Plug 'vim-airline/vim-airline'
  Plug 'vim-airline/vim-airline-themes'
  Plug 'whatyouhide/vim-lengthmatters'
  Plug 'rhlobo/vim-super-retab'

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

" let mapleader = ','
" let maplocalleader = '\'

call Plug()

let g:syntastic_sh_shellcheck_args = "-x"

let g:airline_powerline_fonts = 1

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
set tabstop=20
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
"autocmd BufNewFile,BufRead *.v set filetype=go

" https://csswizardry.com/2017/01/preparing-vim-for-apples-touch-bar/
inoremap jj <esc>
inoremap kk <esc>
inoremap jk <esc>
inoremap kj <esc>

nnoremap <Leader><space> :nohlsearch<Enter>

let g:org_agenda_files=['~/dev/tlcsrc/log/log.org']
