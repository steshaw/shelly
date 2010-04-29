set -x # debug on

# Make sure prompt is exported to subshells.
export PS1

autoapps=~/bin/autoapps
[[ -f ${autoapps} ]] && . ${autoapps}

# MacPorts Installer addition on 2010-04-29_at_06:37:58: adding an appropriate PATH variable for use with MacPorts.
export PATH=/opt/local/bin:/opt/local/sbin:$PATH
# Finished adapting your PATH environment variable for use with MacPorts.

# On Mac OS seem to need to explicitly call the .bashrc
bashrc=~/.bashrc
[[ $(uname) == 'Darwin' && -f ${bashrc} ]] && . ${bashrc}

set +x # debug off
