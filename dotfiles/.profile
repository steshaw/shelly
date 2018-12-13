#!/usr/bin/env bash

export SHELLY_DEV_DIR=~/dev
export SHELLY_HOME=${SHELLY_DEV_DIR}/steshaw/shelly

SHELLY_NOISY=${SHELLY_NOISY:-false}

# shellcheck source=etc/functions.sh
source $SHELLY_HOME/etc/functions.sh

Echo Executing ~/.profile

function prettyPath {
  if [[ $SHELLY_NOISY != false ]] && hasTty; then
    echo "$@" "{"
    echo "$PATH" | tr : '\n' | perl -pe 's/^/  /'
    echo "}"
  fi
}

prettyPath "PATH (before) ="

#
# On Mac, this is required.
#
if [[ -x /usr/libexec/path_helper ]]; then
  Echo "PATH (before path_helper) $PATH"
  eval "$(/usr/libexec/path_helper -s)"
  Echo "PATH (after  path_helper) $PATH"
fi

#
# FIX ${SHELL}
#
Echo "SHELL (before) = ${SHELL}"
if isBash; then
  SHELL="$(command -v bash)"
elif isZsh; then
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
# Initialise preexec/precmd functions.
#
# NOTE: This must come before sourcing ~/.profile.d/* because some of those will
# use register preexec/precmd functions
#
# shellcheck disable=SC2034
preexec_functions=()
# shellcheck disable=SC2034
precmd_functions=()

#
# Source ~/.profile.d/*
#
unset PROMPT_COMMAND # In case of `source ~/.profile`.
for file in ~/.profile.d/*; do
  sourceExists "${file}"
done

#
# Set up bash-preexec.
# See https://github.com/rcaloras/bash-preexec.
#
if isBash; then
  unset __bp_imported # In case of `source ~/.profile`.
  sourceExists $SHELLY_HOME/etc/bash-preexec.sh
fi

#
# Add user's private bins to PATH.
#
prependPaths ~/.local/bin ~/bin

prettyPath "PATH (after) ="
macos-sync-env PATH

sourceExists ~/.profile.local

#
# Explicitly source `.bashrc`.
#
if isBash && hasTty; then
  sourceExists ~/.bashrc
fi
