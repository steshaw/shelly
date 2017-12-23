#!/usr/bin/env bash

if [ "$SHLVL" = 1 ]; then
  # when leaving the console clear the screen to increase privacy
  [[ -x /usr/bin/clear_console ]] && /usr/bin/clear_console -q
fi

if [[ -n $SSH_AGENT_PID ]]; then
  ssh-add -D
  ssh-agent -k
fi
