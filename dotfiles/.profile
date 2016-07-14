#!/usr/bin/env bash

#set -eu

echo Executing ~/.profile

function prettyPath {
  echo "{"
  echo $PATH | tr : '\n' | perl -pe 's/^/  /'
  echo "}"
}

echo "PATH (before) = \c"
prettyPath

source ~/.functions

#
# FIX ${SHELL}
#
echo "SHELL (before) = ${SHELL}"
if [[ -n ${BASH_VERSION:-} ]]; then
  SHELL="$(which bash)"
elif [[ -n ${ZSH_VERSION:-} ]]; then
  SHELL="$(which zsh)"
fi
echo "SHELL (after)  = ${SHELL}"

#
# Setup shelly path.
#
SHELLY_BIN=~/Projects/steshaw/shelly/bin
source ${SHELLY_BIN}/ShellyPath
prependPaths ${SHELLY_BIN}

#
# Homebrew
#
# Some other configuration rely on brew being in the PATH, so it must be early.
#
prependPaths /usr/local/bin /usr/local/sbin
if [ -x "$(which brew)" ]; then
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

echo "PATH (after) = \c"
prettyPath
sync-env-to-plist PATH
