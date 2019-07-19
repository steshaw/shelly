export SHELLY_DEV_DIR=~/Code
export SHELLY_HOME=${SHELLY_DEV_DIR}/steshaw/shelly

source $SHELLY_HOME/etc/functions.sh

Echo "Executing ~/.zprofile"

Echo "Delegating to ~/.profile"
source ~/.profile
