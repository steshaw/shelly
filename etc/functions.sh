#!/usr/bin/env bash

prependPaths() {
  for path_ in "$@"; do
    Echo "Prepending ${path_}"
    PATH=${path_}:${PATH}
  done
}

prependPathsExists() {
  for path_ in "$@"; do
    if [[ -r "$path_" ]]; then
      Echo "Prepending ${path_}"
      PATH=${path_}:${PATH}
    fi
  done
}

sourceExists() {
  for path_ in "$@"; do
    if [[ -e ${path_} ]];then
      Echo "Sourcing $path_"
      # shellcheck disable=SC1090
      source "${path_}"
    fi
  done
}

firstDirectoryExists() {
  for dir in "$@"; do
    [[ -d ${dir} ]] && echo "${dir}"
  done
  echo ""
}

homeFromBin() {
  command=$1
  bin=$(command -v "$command")
  if [ -n "$bin" ]; then
    bin=$(realpath "$bin")
    dirname "$(dirname "$bin")"
  fi
}

has() {
  command=$1
  command -v "$command" >/dev/null
}

isBash() {
  [[ -n ${BASH_VERSION-} ]]
}

isZsh() {
  [[ -n ${ZSH_VERSION-} ]]
}

hasTty() {
  [[ -t 1 ]]
}

shellyIsNoisy() {
  [[ $SHELLY_NOISY == true || $SHELLY_NOISY == 1 ]]
}

shellyShouldEcho() {
  shellyIsNoisy && hasTty
}

# Only echo when we have a tty. This is so that scp will work.
Echo() {
  shellyShouldEcho && echo "$@"
}

thisFile() {
  local source abs
  if isZsh; then
    # shellcheck disable=SC2154
    source=$(echo "$funcfiletrace" | cut -d: -f1)
    abs=$source:A
  else
    source=$(caller | cut -d' ' -f2)
    abs=$(realpath "${source}")
  fi
  echo "$abs"
}
