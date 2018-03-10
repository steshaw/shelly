#!/usr/bin/env bash

#set -eu
[[ -r ~/.functions ]] && source ~/.functions

Echo Executing ~/.profile

unset PROMPT_COMMAND

function prettyPath {
  Echo "{"
  Echo $PATH | tr : '\n' | perl -pe 's/^/  /'
  Echo "}"
}

Echo -e "PATH (before) = \c"
prettyPath


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
  SHELL="$(which bash)"
elif [[ -n ${ZSH_VERSION:-} ]]; then
  SHELL="$(which zsh)"
fi
Echo "SHELL (after)  = ${SHELL}"

#
# Setup shelly path.
#
SHELLY_HOME=~/Projects/steshaw/shelly
SHELLY_BIN=${SHELLY_HOME}/bin
source ${SHELLY_BIN}/ShellyPath
prependPaths ${SHELLY_BIN}

#
# Homebrew
#
# Some other configuration rely on brew being in the PATH, so it must be early.
#
prependPaths /usr/local/bin /usr/local/sbin
if [ -x "$(which brew 2>&-)" ]; then
  export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig
  sync-env-to-plist PKG_CONFIG_PATH
fi

#
# Source ~/.profile.d/*
#
for file in ~/.profile.d/*; do
  sourceExists "${file}"
done

#
# Add ~/bin to PATH
#
prependPaths ~/bin

#
# Explicitly call the .bashrc
#
# if running bash
if [[ -n ${BASH_VERSION:-} ]]; then
  sourceExists ~/.bashrc
fi

Echo -e "PATH (after) = \c"
prettyPath
sync-env-to-plist PATH
