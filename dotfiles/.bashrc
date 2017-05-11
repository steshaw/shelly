#!/usr/bin/env bash

set -u

echo Executing ~/.bashrc

[ -r ~/.functions ] && source ~/.functions
sourceExists /etc/skel/.bashrc

#
# bash-completions + git-prompt.
#
set +u
if which brew 2>&-; then
  # if using bash-completion version 1.
  sourceExists "$(brew --prefix)/etc/bash_completion"
  # For bash-completion@2.
  sourceExists "$(brew --prefix)/share/bash-completion/bash_completion"
#  complete -D -o bashdefault -o default
else
  sourceExists /etc/profile.d/bash_completion.sh
  sourceExists /etc/bash_completion.d/git-prompt.sh
  sourceExists /usr/share/git/git-prompt.sh # Works on MSYS2 when bash-completion is installed
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
    local UPC=${EMR}       # user's color
    local UP='\$'         # user's prompt
  else
    local UPC=${EMG}       # user's color
    local UP='➯'          # user's prompt
  fi

  UC="${EMC}" # user@host colour
  DC="${EMB}" # pwd colour
  SC="${EMM}" # separator colour
  GC="${EMY}" # git prompt colour

  PS1="${TITLEBAR}\n${UC}\${debian_chroot:+(${debian_chroot:-})}\u@\h${SC}:${DC}\w${GC}\$(set +u; __git_ps1; set -u)\n${UPC}${UP}${NONE} "
}
bash_prompt

function setTabTitle() {
  local base
  base=$(basename "${PWD}")
  echo -ne "\033]0;${base}\007"
}
PROMPT_COMMAND="setTabTitle; ${PROMPT_COMMANDL:-}"

# FIXME : Unfortunately something goes wrong on MSYS2 but this bash prompt file
# FIXME : is a big hack anyhow.
if [[ ${OSTYPE} = msys ]]; then
  PROMPT_COMMAND='__git_ps1 "\n\u@\h:\w" \\n"\\\$ "'
fi

function shellOption {
  option=$1
  shopt -s ${option} || echo "Cannot set '${option}', perhaps you are not running Bash 4.x.x?"
}

shellOption globstar 2>/dev/null # Use globstar if we're using Bash > 4.0
shellOption nullglob
shellOption autocd
shellOption xpg_echo

# Prevent noclobber in a Nix shell because it causes Nix trouble overwriting tmp files.
if [[ $- == *i* && -z ${IN_NIX_SHELL:-} ]]; then
  set -o noclobber
fi

CDPATH=.:~/Projects

# Google Cloud
sourceExists '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.bash.inc'
sourceExists '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.bash.inc'

# Kubernetes
if which kubectl >/dev/null; then
  source <(kubectl completion bash)
fi

if which hub >/dev/null; then
  alias git=hub
fi

sourceExists ~/.shrc

set +u
true
