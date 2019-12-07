#!/usr/bin/env bash

export SHELLY_DEV_DIR=~/Code
export SHELLY_HOME=${SHELLY_DEV_DIR}/steshaw/shelly
# shellcheck disable=SC2034
SHELLY_NOISY=0 # Set to 1 for crude debugging.

profile_startup=1
profile_startup_type='timestamp'
profile_startup_type='xtracefd'
profile_startup_type='incremental'

if [[ ${profile_startup} -eq 1 ]]; then

  timestamp=$(date +%Y%m%d-%H%M%S)
  log="/tmp/login.${profile_startup_type}.${USER}.${timestamp}.log"
  unset timestamp

  if [[ ${profile_startup_type} == 'timestamp' ]]; then
    # Adapted from https://stackoverflow.com/a/5015179/482382
    original_PS4="$PS4"
    PS4='+ $EPOCHREALTIME\011 '
    timestamp=$(date +%Y%m%d-%H%M%S)
    exec 3>&2 # Save stderr as fd 3.
    exec 2>"${log}" # Redirect stderr.
    set -x
  fi

  if [[ ${profile_startup_type} == 'incremental' ]]; then
    ts=~/.nix-profile/bin/ts
    # Ensure ts exists and accepts -i.
    if [[ -x ${ts} ]] && ${ts} -i </dev/null 2>/dev/null; then
      exec 3>&1 # Save stdout.
      exec 4>&2 # Save stderr.
      exec &> >(${ts} -i '%.s' >"${log}")
      unset ts
      set -x
    else
      unset profile_startup_type
    fi
    unset ts
  fi

  if [[ ${profile_startup_type} == 'xtracefd' ]]; then
    ts=~/.nix-profile/bin/ts
    # Ensure ts exists and accepts -i.
    if [[ -x ${ts} ]] && ${ts} -i </dev/null 2>/dev/null; then
      # HT https://mdjnewman.me/2017/10/debugging-slow-bash-startup-files/
      exec 3> >(${ts} -i '%.s' >"$log")
      # https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html
      export BASH_XTRACEFD=3
      set -x
    else
      unset profile_startup_type
    fi
    unset ts
  fi

  unset log
fi

# shellcheck disable=SC1090
source $SHELLY_HOME/etc/functions.sh
shelly_determine_os

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

# Synchronise environment variables with macOS GUI programs.
[[ ${SHELLY_OS} == 'darwin' ]] && macos-environment-sync --install

if [[ ${profile_startup} -eq 1 ]]; then
  unset profile_startup
  if [[ ${profile_startup_type} == 'timestamp' ]]; then
    set +x
    PS4=${original_PS4}
    unset original_PS4
    exec 2>&3 # Restore fd 2 as stderr.
    exec 3>&- # Close fd 3.
  elif [[
      ${profile_startup_type} == 'simple' ||
      ${profile_startup_type} == 'incremental'
  ]]; then
    set +x
    exec 1>&3 # Restore fd 1 as stdout.
    exec 3>&- # Close fd 3.
    exec 2>&4 # Restore fd 2 as stderr.
    exec 4>&- # Close fd 4.
  elif [[ ${profile_startup_type} == 'xtracefd' ]]; then
    set +x
    unset BASH_XTRACEFD
  else
    set +x
  fi
  unset profile_startup_type
fi
