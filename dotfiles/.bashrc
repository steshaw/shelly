
# TODO: add the colors like in Ubuntu prompt.
# 3 line prompt: newline + user@host + regular prompt (i.e. $ or #).
PS1='\n\u@\h: \w\n\$ '

# vi[m] > emacs :)
set -o vi

alias ls='ls -FGh'
alias l='ls -l'
alias ll='l -a'

# Make file ops safer.
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'

alias path='echo $PATH | tr : "\n"'
