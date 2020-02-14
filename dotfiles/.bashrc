#!/usr/bin/env bash

if [[ ! -t 0 ]]; then
  return
fi
if [[ $IN_NIX_SHELL == pure ]]; then
  return
fi

if [[ -z $SHELLY_HOME ]]; then
  export SHELLY_DEV_DIR=~/Code
  export SHELLY_HOME=${SHELLY_DEV_DIR}/steshaw/shelly
fi

# shellcheck disable=SC1090
source $SHELLY_HOME/etc/functions.sh

Echo Executing ~/.bashrc

#
# Set up bash-preexec.
# See https://github.com/rcaloras/bash-preexec.
#
if isBash; then
  unset __bp_imported # In case of `source ~/.profile`.
  sourceExists $SHELLY_HOME/etc/bash-preexec.sh
fi

sourceExists /etc/skel/.bashrc
sourceExists ~/.iterm2_shell_integration.bash

#
# bash-completion
#

trySource () {
  file=$1
  [[ -r $file ]] && shellySource "$file"
}

dir=~/.nix-profile/etc/bash_completion.d
if [[ -e ${dir} ]]; then
  Echo "Setting BASH_COMPLETION_COMPAT_DIR=${dir}"
  # shellcheck disable=SC2034
  BASH_COMPLETION_COMPAT_DIR=${dir}
fi
unset dir

# Enable bash-completions when not on NixOS.
# This requires setting XDG_DATA_DIRS.
if [[ ! -f /etc/NIXOS ]]; then
  export XDG_DATA_DIRS=~/.nix-profile/share:${XDG_DATA_DIRS:-/usr/local/share:/usr/share}
fi

trySource ~/.nix-profile/etc/profile.d/bash_completion.sh ||
  trySource /etc/profile.d/bash_completion.sh ||
  trySource "${HOMEBREW_PREFIX}/etc/profile.d/bash_completion.sh"

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
}
if true; then
  precmd_functions+=(saveHistory)
fi

# Prevent noclobber in a Nix shell because it causes Nix trouble overwriting tmp files.
if [[ $- == *i* && -z ${IN_NIX_SHELL:-} ]]; then
  set -o noclobber
fi

# shellcheck disable=SC1090
source $SHELLY_HOME/etc/shrc

if has hub; then
  alias git=hub
fi

# If not running interactively, return
case $- in
  *i*) ;;
  *) return;;
esac

sourceExists /google/devshell/bashrc.google

for file in ~/.config/bashrc.d/*; do
  sourceExists "${file}"
done
