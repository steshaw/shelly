#!/usr/bin/env zsh

#set -eu

echo "Executing ~/.zprofile"

# Set up bash completion.
autoload -U +X compinit && compinit
autoload -U +X bashcompinit && bashcompinit

echo "Delegating to ~/.profile"
source ~/.profile
