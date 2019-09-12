echom "after/org.vim"
"call SyntaxRange#Include('#+begin_src timedot', '#+end_src', 'timedot', 'comment')

" In perl.vim:
":syntax include @Pod <sfile>:p:h/pod.vim
"":syntax region perlPOD start="^=head" end="^=cut" contains=@Pod

syntax include @Timedot syntax/timedot.vim
syntax region orgTimedot start="#+begin_src" end="#+end_src" contains=@Timedot
