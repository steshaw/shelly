echo Executing .bashrc…
echo dollar0=$0

# TODO: add the colors like in Ubuntu prompt.
# 3 line prompt: newline + user@host + regular prompt (i.e. $ or #).
export PS1='\n\u@\h: \w\n\$ '

export EDITOR=/usr/bin/vim

# Vim > Emacs :)
set -o vi

#alias ls='ls --color -FGh'
alias ls='ls -GFh'
alias l='ls -l'
alias ll='l -a'

alias wi='type -ap'

# Make file ops safer.
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'

alias cx='chmod +x'
alias cw='chmod +w'

alias vbp='vim ~/.bash_profile'
alias sbp='source ~/.bash_profile'
alias vbrc='vim ~/.bashrc'
alias sbrc='source ~/.bashrc'

alias path='echo $PATH | tr : "\n"'

alias sml='rlwrap sml'
