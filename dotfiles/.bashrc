#!/usr/bin/env bash

set -u

echo Executing ~/.bashrc

sourceExists /etc/skel/.bashrc

#
# bash-completions
#
set +u
if [ -x "$(which brew)" ]; then
  sourceExists "$(brew --prefix)/share/bash-completion/bash_completion" || sourceExists "$(brew --prefix)/etc/bash_completion"
  sourceExists "$(brew --prefix)/etc/bash_completion.d/git-prompt.sh"
else
  sourceExists /etc/profile.d/bash_completion.sh
  sourceExists /etc/bash_completion.d/git-prompt.sh
  sourceExists /nix/var/nix/profiles/default/etc/profile.d/bash_completion.sh
  sourceExists ~/.nix-profile/etc/bash_completion.d/git-prompt.sh
fi
set -u

bash_prompt() {
  case $TERM in
    xterm*|rxvt*)
      local TITLEBAR='\[\033]0;\u@\h:\w\007\]'
      ;;
    *)
      local TITLEBAR=""
      ;;
  esac
  local NONE="\[\033[0m\]"  # unsets color to term's fg color

  # regular colors
  # shellcheck disable=SC2034
  local K="\[\033[0;30m\]"  # black
  # shellcheck disable=SC2034
  local R="\[\033[0;31m\]"  # red
  # shellcheck disable=SC2034
  local G="\[\033[0;32m\]"  # green
  # shellcheck disable=SC2034
  local Y="\[\033[0;33m\]"  # yellow
  # shellcheck disable=SC2034
  local B="\[\033[0;34m\]"  # blue
  # shellcheck disable=SC2034
  local M="\[\033[0;35m\]"  # magenta
  local C="\[\033[0;36m\]"  # cyan
  # shellcheck disable=SC2034
  local W="\[\033[0;37m\]"  # white

  # emphasized (bolded) colors
  # shellcheck disable=SC2034
  local EMK="\[\033[1;30m\]"
  # shellcheck disable=SC2034
  local EMR="\[\033[1;31m\]"
  local EMG="\[\033[1;32m\]"
  # shellcheck disable=SC2034
  local EMY="\[\033[1;33m\]"
  local EMB="\[\033[1;34m\]"
  # shellcheck disable=SC2034
  local EMM="\[\033[1;35m\]"
  # shellcheck disable=SC2034
  local EMC="\[\033[1;36m\]"
  # shellcheck disable=SC2034
  local EMW="\[\033[1;37m\]"

  # background colors
  # shellcheck disable=SC2034
  local BGK="\[\033[40m\]"
  # shellcheck disable=SC2034
  local BGR="\[\033[41m\]"
  # shellcheck disable=SC2034
  local BGG="\[\033[42m\]"
  # shellcheck disable=SC2034
  local BGY="\[\033[43m\]"
  # shellcheck disable=SC2034
  local BGB="\[\033[44m\]"
  # shellcheck disable=SC2034
  local BGM="\[\033[45m\]"
  # shellcheck disable=SC2034
  local BGC="\[\033[46m\]"
  # shellcheck disable=SC2034
  local BGW="\[\033[47m\]"

  if [[ $(id -u) -eq "0" ]]; then
    local UC=${EMR}       # user's color
    local UP='\$'         # user's prompt
  else
    local UC=${EMG}       # user's color
    local UP='➯'          # user's prompt
  fi

  DC="${EMB}" # pwd colour
  SC="${EMM}" # separator colour
  GC="${EMY}" # git prompt colour

  PS1="${TITLEBAR}\n${UC}\${debian_chroot:+(${debian_chroot:-})}\u@\h${SC}:${DC}\w${GC}$(set +u; __git_ps1; set -u)\n${UC}${UP}${NONE} "
}
bash_prompt

function setTabTitle() {
  local base
  base=$(basename "${PWD}")
  echo -ne "\033]0;${base}\007"
}
PROMPT_COMMAND="setTabTitle; ${PROMPT_COMMANDL:-}"

export EDITOR
EDITOR=$(which vim)
alias e='${EDITOR}'

set -o vi

#alias ls='ls --color -Fh'
alias ls='ls -GFh'
alias l='ls -l'
alias ll='l -A'

alias wi='type -ap'

# Make file ops safer.
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'

alias cx='chmod +x'
alias cw='chmod +w'

alias vbp='vim ~/.profile'
alias sbp='source ~/.profile'
alias vbrc='vim ~/.bashrc'
alias sbrc='source ~/.bashrc'

alias path='echo $PATH | tr : "\n"'
alias macpath='defaults read ~/.MacOSX/environment PATH | tr : "\n"'

if [[ "$(uname)" != 'Darwin' ]]; then
  alias pbcopy='xsel --clipboard --input'
  alias pbpaste='xsel --clipboard --output'
fi

alias sml='rlwrap sml'
alias ocaml='rlwrap ocaml'

if [ -x "$(which gnome-open)" ]; then
  alias o='gnome-open'
elif [ -x "$(which xdg-open)" ]; then
  alias o='xdg-open'
else
  alias o='open'
fi

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

bashrcLocal=~/.bashrc.local
[ -f "${bashrcLocal}" ] && source "${bashrcLocal}"

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

shopt -s globstar 2>/dev/null

# added by travis gem
[ -f /Users/steshaw/.travis/travis.sh ] && source /Users/steshaw/.travis/travis.sh

set +u

true
