#!/usr/bin/env bash

export SHELLY_DEV_DIR=~/dev
SHELLY_HOME=${SHELLY_DEV_DIR}/steshaw/shelly

init() {
  local fns=$SHELLY_HOME/etc/functions.sh
  # shellcheck source=etc/functions.sh
  [[ -r $fns ]] && source $fns
}
init
unset -f init

Echo Executing ~/.profile

#unset PROMPT_COMMAND

function prettyPath {
  Echo "$@" "{"
  Echo "$PATH" | tr : '\n' | perl -pe 's/^/  /'
  Echo "}"
}

prettyPath "PATH (before) = "

#
# On Mac, this is required.
#
if [[ -x /usr/libexec/path_helper ]]; then
  Echo "PATH (before path_helper) $PATH"
  eval `/usr/libexec/path_helper -s`
  Echo "PATH (after  path_helper) $PATH"
fi

#
# FIX ${SHELL}
#
Echo "SHELL (before) = ${SHELL}"
if [[ -n ${BASH_VERSION:-} ]]; then
  SHELL="$(command -v bash)"
elif [[ -n ${ZSH_VERSION:-} ]]; then
  SHELL="$(command -v zsh)"
fi
Echo "SHELL (after)  = ${SHELL}"

#
# Put shelly packages and commands on PATH.
#
anon() {
  local shellyBin=${SHELLY_HOME}/bin
  # shellcheck source=bin/ShellyPath
  source ${shellyBin}/ShellyPath
  prependPaths ${shellyBin}
}
anon
unset -f anon

#
# Homebrew
#
# Some other configuration rely on brew being in the PATH, so it must be early.
#
prependPaths /usr/local/bin /usr/local/sbin
if has brew; then
  export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig
  macos-sync-env PKG_CONFIG_PATH
fi

#
# Source ~/.profile.d/*
#
for file in ~/.profile.d/*; do
  sourceExists "${file}"
done

#
# Add user's private bins to PATH.
#
prependPaths ~/.local/bin ~/bin

prettyPath "PATH (after) = "
macos-sync-env PATH

sourceExists $SHELLY_HOME/etc/bash-preexec.sh
sourceExists ~/.profile.local

#
# Explicitly call the .bashrc
#
# if running bash
if [[ -n ${BASH_VERSION:-} ]]; then
  sourceExists ~/.bashrc
fi
