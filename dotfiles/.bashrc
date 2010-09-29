
autoapps=~/bin/autoapps
[[ -f ${autoapps} ]] && . ${autoapps}

# TODO: add the colors like in Ubuntu prompt.
# 3 line prompt: newline + user@host + regular prompt (i.e. $ or #).
PS1='\n\u@\h: \w\n\$ '

export EDITOR=/usr/bin/vim


# vi[m] > emacs :)
set -o vi

alias ls='ls --color -FGh'
alias l='ls -l'
alias ll='l -a'

# Make file ops safer.
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'

alias cx='chmod +x'
alias cw='chmod +w'

alias path='echo $PATH | tr : "\n"'
