#!/usr/bin/env bash

prependPaths() {
  for path_ in "$@"; do
    if [[ -d ${path_} ]]; then
      Echo "Prepending ${path_}"
      PATH=${path_}:${PATH}
    fi
  done
}

sourceExists() {
  for path_ in "$@"; do
    if [[ -e ${path_} ]];then
      Echo "Sourcing $path_"
      source "${path_}"
    fi
  done
}

firstDirectory() {
  for dir in "$@"; do
    [[ -d ${dir} ]] && echo "${dir}"
  done
  echo ""
}

homeFromBin() {
  command=$1
  bin=$(command -v "$command")
  if [ -n "$bin" ]; then
    bin=$(RealPath "$bin")
    dirname "$(dirname "$bin")"
  fi
}

has() {
  command=$1
  command -v "$command" >/dev/null
}

# Only echo when we have a tty. This is so that scp will work.
Echo() {
  [[ -n $PS1 ]] && echo "$@"
}
