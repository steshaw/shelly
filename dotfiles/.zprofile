export SHELLY_DEV_DIR=~/Code
export SHELLY_HOME=${SHELLY_DEV_DIR}/steshaw/shelly

# shellcheck disable=SC1090
source ${SHELLY_HOME}/scripts/functions.sh

Echo "Executing ~/.zprofile"

Echo "Delegating to ~/.profile"
# shellcheck disable=SC1090
source ~/.profile
