#!/usr/bin/env bash

export SHELLY_DEV_DIR=~/Code
export SHELLY_HOME=${SHELLY_DEV_DIR}/steshaw/shelly
# shellcheck disable=SC2034
SHELLY_NOISY=0 # Set to 1 for crude debugging.

# shellcheck disable=SC1090
source $SHELLY_HOME/etc/functions.sh

Echo Executing ~/.profile

prettyPath() {
  if shellyShouldEcho; then
    echo "$@" "{"
    $SHELLY_HOME/scripts/ppath | perl -pe 's/^/  /'
    echo "}"
  fi
}

prettyPath ".profile PATH top"

if isZsh; then
  # Set up bash completion.
  autoload -U +X compinit && compinit
  autoload -U +X bashcompinit && bashcompinit
fi

prependPaths ${SHELLY_HOME}/scripts

prettyPaths() {
  if shellyShouldEcho; then
    echo "$@" "{"
    paths | perl -pe 's/^/  /'
    echo "}"
  fi
}

#
# Shelly packages — if any.
#
# shellcheck disable=SC1090
source ${SHELLY_HOME}/etc/shelly-pkgs-env.sh

#
# Initialise preexec/precmd functions.
#
# NOTE: This must come before sourcing ~/.profile.d/* because some of
# those will use register preexec/precmd functions.
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
# Add user's private bins to PATH.
#
prependPaths ~/.local/bin
prependPathsExists ~/bin

prettyPaths ".profile: After .profile.d/* and private bins"

sourceExists ~/.profile.local

# Ensure xterm-24bit exists.
if isInteractive && [[ -z $(ls ~/.terminfo/*/xterm-24bit 2>/dev/null) ]]; then
  xterm-24bit-create
fi

# Use 24bit terminal for tmux. A hack that works?
if isInteractive && [[ $TERM == screen ]]; then
  TERM=xterm-24bit
fi

#
# Explicitly source `.bashrc`.
#
if isBash && isInteractive; then
  sourceExists ~/.bashrc
fi

prettyPaths ".profile: After .bashrc"

# Dedup PATH.
PATH=$(dedup-path PATH)

prettyPaths ".profile: After dedup"

export XXX=xxx
export FRED=fred
export BARNEY=barney
export WILMA=wilma
export BETTY=betty

# To synchronise environment variables with macOS GUI programs.
macos-environment-sync --install
