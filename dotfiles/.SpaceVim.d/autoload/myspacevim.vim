function! myspacevim#before() abort
  echom "myspacevim#before"
  set rtp+=~/Code/steshaw/vim-timedot
endfunction

function! myspacevim#after() abort
"  echom "myspacevim#after"
  call SyntaxRange#Include('#+begin_src timedot', '#+end_src', 'timedot', 'comment')
"  syntax include @Timedot syntax/timedot.vim
"  syntax region orgTimedot start="#+begin_src" end="#+end_src" contains=@Timedot
endfunction

function! myspacemacs#fixTimedot() abort
  syntax include @Timedot syntax/timedot.vim
  syntax region orgTimedot start="#+begin_src" end="#+end_src" contains=@Timedot
endfunction
