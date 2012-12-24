#
# .bashrc
#

skeletonBashrc=/etc/skel.bashrc
[[ -f ${skeletonBashrc} ]] && . ${skeletonBashrc}

# 3 line prompt: newline + user@host + regular prompt (i.e. $ or #).
#export PS1='\n\u@\h: \w\n\$ '
export PS1='\n${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\] \w\[\033[00m\]\n\$ '

export EDITOR=/usr/bin/vim

set -o vi

alias ls='ls --color -Fh'
#alias ls='ls -GFh'
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
alias macpath='defaults read ~/.MacOSX/environment PATH | tr : "\n"'

alias sml='rlwrap sml'
alias ocaml='rlwrap ocaml'

if [[ -x $(which gnome-open) ]]; then
  alias o='gnome-open'
elif [[ -x $(which xdg-open) ]]; then
  alias o='xdg-open'
else
  alias o='open'
fi

# These are dangerous.
#alias git-svn-up='git stash && git svn rebase && git stash pop'
#alias git-svn-commit='git stash && git svn dcommit && git stash pop'

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

[[ -f ~/.bashrc.local ]] && source ~/.bashrc.local
