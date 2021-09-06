#!/usr/bin/env bash

# This also ensures that we switch the "standard" oh-my-zsh ls aliases
# as we want the shortest one for the most common command! i.e. l.

enable_exa=1
enable_lsd=0

if [[ $enable_exa -ne 0 ]] && has exa; then
  alias ls='exa --classify'
  alias l='ls --long'
  alias ll='l --all'
  # Fallback to GNU ls.
  alias la='ll --all'
else
  if [[ $enable_lsd -ne 0 ]] && has lsd; then
    alias ls=lsd
    alias l='ls -l --human-readable'
    alias ll='l --almost-all'
    alias la='l --all'
  elif ls --color >/dev/null 2>&1; then
    # GNU ls.
    alias ls='ls --color --classify'
    alias l='ls -l --human-readable'
    alias ll='l --almost-all'
    alias la='l --all'
  else
    # macOS/Darwin/BSD ls.
    export CLICOLOR=
    if has dircolors; then
      eval "$(dircolors)"
      LSCOLORS=$(gnu2bsd-colors)
      export LSCOLORS
    fi
    alias ls='ls -F'
    alias l='ls -l -h'
    alias ll='l -A'
    alias la='l -a'
  fi
fi
