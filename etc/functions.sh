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

shellySource() {
  file=$1
  Echo "Sourcing $file"
  # shellcheck disable=SC1090
  source "${file}"
}

sourceExists() {
  for file in "$@"; do
    if [[ -e ${file} ]];then
      shellySource "$file"
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
  type -p "$command" >/dev/null
}

isBash() {
  [[ -n ${BASH_VERSION-} ]]
}

isZsh() {
  [[ -n ${ZSH_VERSION-} ]]
}

isInteractive() {
  [[ -t 0 ]]
}

shellyIsNoisy() {
  [[ $SHELLY_NOISY == true || $SHELLY_NOISY == 1 ]]
}

shellyShouldEcho() {
  shellyIsNoisy && isInteractive
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

shelly_determine_os() {
  # shellcheck disable=SC2034
  if [[ ${OSTYPE} == darwin* ]]; then
    SHELLY_OS='darwin'
  else
    SHELLY_OS=${OSTYPE}
  fi
}
