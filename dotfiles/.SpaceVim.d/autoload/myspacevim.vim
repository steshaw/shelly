function! myspacevim#before() abort
"  echom "Inside myspacevim#before"
  set rtp+=~/Code/steshaw/vim-timedot
endfunction

function! myspacevim#after() abort
  echom "Inside myspacevim#after"
  call myspacevim#fixTimedotOld()
endfunction

function! myspacevim#fixTimedotOld() abort
  echom "Inside myspacevim#fixTimedotOld"
  call SyntaxRange#Include('#+begin_src timedot', '#+end_src', 'timedot', 'comment')
endfunction

" FIXME: Would like to use something like this to replace the "Old"
" FIXME: function (which relies on the SyntaxRange plugin).
function! myspacevim#fixTimedotNew() abort
  echom "Inside myspacevim#fixTimedotNew"
  syntax include @Timedot syntax/timedot.vim
  syntax region orgTimedot start='#+begin_src timedot' end='#+end_src' contains=@Timedot
endfunction
