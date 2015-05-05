#
# .profile
#

prependPaths() {
  for path_ in "$@"; do
    [[ -d ${path_} ]] && PATH=${path_}:${PATH}
  done
}

sourceExists() {
  for path_ in "$@"; do
    [[ -e ${path_} ]] && source ${path_}
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
  bin=$(which $command)
  if [ -n "$bin" ]; then
    bin=$(RealPath $bin)
    echo $(dirname $(dirname $bin))
  fi
}

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
  sourceExists ${file}
done

#
# Add ~/bin to PATH
#
prependPaths ~/bin

#
# Explicitly call the .bashrc
#
# if running bash
if [[ -n "$BASH_VERSION" ]]; then
  sourceExists ~/.bashrc
fi

sync-env-to-plist PATH
