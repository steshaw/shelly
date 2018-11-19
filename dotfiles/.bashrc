#!/usr/bin/env bash

if [[ -z $SHELLY_HOME ]]; then
  export SHELLY_DEV_DIR=~/dev
  export SHELLY_HOME=${SHELLY_DEV_DIR}/steshaw/shelly
fi

# shellcheck source=etc/functions.sh
source $SHELLY_HOME/etc/functions.sh

Echo Executing ~/.bashrc

sourceExists /etc/skel/.bashrc
sourceExists ~/.iterm2_shell_integration.bash

#
# bash-completions + git-prompt.
#
if has brew; then
  if [[ $BASH_VERSION == 4.* ]]; then
    # requires `brew install bash-completion@2`.
    sourceExists "$(brew --prefix)/share/bash-completion/bash_completion"
  elif [[ $BASH_VERSION == 3.* ]]; then
    base="$(brew --prefix)/Cellar/bash-completion/1.3_3/etc"
    BASH_COMPLETION="$base/bash_completion"
    BASH_COMPLETION_DIR="$base/bash_completion.d"
#    set -x
#    sourceExists "$base/etc/bash_completion"
    sourceExists "$base/profile.d/bash_completion.sh"
#    set +x
  else
    # Make a best effort to find the appropriate bash-completion files.

    # if using bash-completion version 1.
    sourceExists "$(brew --prefix)/etc/bash_completion"
    # For bash-completion@2.
    sourceExists "$(brew --prefix)/share/bash-completion/bash_completion"
    # complete -D -o bashdefault -o default
  fi
else
  sourceExists /etc/profile.d/bash_completion.sh
  sourceExists /etc/bash_completion.d/git-prompt.sh
  sourceExists /usr/share/git/git-prompt.sh # Works on MSYS2 when bash-completion is installed
  sourceExists /nix/var/nix/profiles/default/etc/profile.d/bash_completion.sh
  sourceExists ~/.nix-profile/etc/bash_completion.d/git-prompt.sh
fi

bash_prompt() {
  [[ -z $PS1 ]] && return

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
    local UPC=${R}        # user's color
    local UP='\#'         # user's prompt
  else
    local UPC=${G}        # user's color
    local UP='➯'          # user's prompt
    local UP='➮'          # user's prompt
    local UP='➭'          # user's prompt
    local UP='➩'          # user's prompt
    local UP='➪'          # user's prompt
    local UP='→'          # user's prompt
    local UP='→'          # user's prompt
    local UP='➝'          # users' prompt
    local UP='\$'         # user's prompt
  fi

  local UC="${C}" # user@host colour
  local DC="${EMB}" # pwd colour
  local SC="${M}" # separator colour
  local GC="${Y}" # git prompt colour

  # Avoid some arrow characters when running under Google Cloud Shell.
  if [[ -n "$DEVSHELL_IP_ADDRESS" ]]; then
    local TOP_LEFT='┌'
    local BOT_LEFT='└'
  else
    local TOP_LEFT='╭'
    local BOT_LEFT='╰'
  fi

  local PROMPT_LINE_1="${UPC}${TOP_LEFT}─${UC}\${debian_chroot:+(${debian_chroot:-})}\u@\h${SC}:${DC}\w"
  local PROMPT_LINE_2="${UPC}${BOT_LEFT}─${UP}${NONE} "
  local iterm2Mark="\[$(iterm2_prompt_mark)\]"

  GIT_PS1_1="${TITLEBAR}\n${PROMPT_LINE_1}"
  GIT_PS1_2="\n${iterm2Mark}${PROMPT_LINE_2}"
  GIT_PS1_3=" ${GC}[%s${GC}]"

  GIT_PS1_SHOWDIRTYSTATE=true
  GIT_PS1_SHOWSTASHSTATE=true
  GIT_PS1_SHOWUNTRACKEDFILES=true
  GIT_PS1_SHOWUPSTREAM='auto'
  GIT_PS1_SHOWCOLORHINTS=true

  setGitPrompt() {
    if [[ ${OSTYPE} = msys ]]; then
      # FIXME: Set a simpler prompt for MSYS2 as the regular one doesn't work.
      __git_ps1 "\n\u@\h:\w" \\n"\\\$ "
    else
      __git_ps1 "${GIT_PS1_1}" "${GIT_PS1_2}" "${GIT_PS1_3}"
    fi
  }
  precmd_functions+=(setGitPrompt)
}
bash_prompt
unset -f bash_prompt

[[ $BASH_VERSION == 4.* ]] && shopt -s globstar
[[ $BASH_VERSION == 4.* ]] && shopt -s autocd
shopt -s xpg_echo

#
# Enable a shared history file for all shell sessions.
#
HISTCONTROL=ignoredups:erasedups
HISTSIZE=100000
HISTFILESIZE=$HISTSIZE
shopt -s histappend
saveHistory() {
  history -a
  history -c
  history -r
}
precmd_functions+=(saveHistory)
#PROMPT_COMMAND="history -a; history -n; ${PROMPT_COMMAND:-}"

# Prevent noclobber in a Nix shell because it causes Nix trouble overwriting tmp files.
if [[ $- == *i* && -z ${IN_NIX_SHELL:-} ]]; then
  set -o noclobber
fi

sourceExists ~/.shrc

# Kubernetes
if has kubectl; then
  source <(kubectl completion bash)
fi

if has hub; then
  alias git=hub
fi

# If not running interactively, return
case $- in
  *i*) ;;
  *) return;;
esac

sourceExists /google/devshell/bashrc.google
