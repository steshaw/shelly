# Tell ShellCheck to use bash checking as zsh is not yet supported.
# https://github.com/koalaman/shellcheck/issues/809
# shellcheck shell=bash

export SHELLY_CODE_DIR=~/Code
export SHELLY_HOME=${SHELLY_CODE_DIR}/steshaw/shelly

# shellcheck disable=SC1090
source ${SHELLY_HOME}/scripts/functions.sh

Echo "Executing ~/.zprofile"

Echo "Delegating to ~/.profile"
# shellcheck disable=SC1090
source ~/.profile
