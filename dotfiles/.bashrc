#
# .bashrc
#

sourceExists /etc/skel/.bashrc

#
# bash-completions
#
if [ -x "$(which brew)" ]; then
  sourceExists "$(brew --prefix)/share/bash-completion/bash_completion" || sourceExists "$(brew --prefix)/etc/bash_completion"
  sourceExists "$(brew --prefix)/etc/bash_completion.d/git-prompt.sh"
else
  sourceExists /etc/profile.d/bash_completion.sh
  sourceExists /etc/bash_completion.d/git-prompt.sh
  sourceExists /nix/var/nix/profiles/default/etc/profile.d/bash_completion.sh
  sourceExists ~/.nix-profile/etc/bash_completion.d/git-prompt.sh
fi

bash_prompt() {
  case $TERM in
    xterm*|rxvt*)
      local TITLEBAR='\[\033]0;\u@\h:\w\007\]'
      ;;
    *)
      local TITLEBAR=""
      ;;
  esac
  local NONE="\[\033[0m\]"  # unsets color to term's fg color

  # regular colors
  local K="\[\033[0;30m\]"  # black
  local R="\[\033[0;31m\]"  # red
  local G="\[\033[0;32m\]"  # green
  local Y="\[\033[0;33m\]"  # yellow
  local B="\[\033[0;34m\]"  # blue
  local M="\[\033[0;35m\]"  # magenta
  local C="\[\033[0;36m\]"  # cyan
  local W="\[\033[0;37m\]"  # white

  # emphasized (bolded) colors
  local EMK="\[\033[1;30m\]"
  local EMR="\[\033[1;31m\]"
  local EMG="\[\033[1;32m\]"
  local EMY="\[\033[1;33m\]"
  local EMB="\[\033[1;34m\]"
  local EMM="\[\033[1;35m\]"
  local EMC="\[\033[1;36m\]"
  local EMW="\[\033[1;37m\]"

  # background colors
  local BGK="\[\033[40m\]"
  local BGR="\[\033[41m\]"
  local BGG="\[\033[42m\]"
  local BGY="\[\033[43m\]"
  local BGB="\[\033[44m\]"
  local BGM="\[\033[45m\]"
  local BGC="\[\033[46m\]"
  local BGW="\[\033[47m\]"

  local UC=$W           # user's color
  [ $UID -eq "0" ] && UC=$R # root's color

#  PS1="$TITLEBAR ${EMK}[${UC}\u${EMK}@${UC}\h ${EMB}\${NEW_PWD}${EMK}]${UC}\\$ ${NONE}"
  # without colors: PS1="[\u@\h \${NEW_PWD}]\\$ "
  # extra backslash in front of \$ to make bash colorize the prompt

  PS1="${TITLEBAR}\n\${debian_chroot:+($debian_chroot)}${C}\u@\h:\w${EMB}$(__git_ps1)\n${EMG}➯ ${NONE}"
}
bash_prompt

function setTabTitle() {
  local base=$(basename "${PWD}")
  echo -ne "\033]0;${base}\007"
}
PROMPT_COMMAND="setTabTitle; $PROMPT_COMMAND"

export EDITOR=$(which vim)
alias e='${EDITOR}'

set -o vi

#alias ls='ls --color -Fh'
alias ls='ls -GFh'
alias l='ls -l'
alias ll='l -A'

alias wi='type -ap'

# Make file ops safer.
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'

alias cx='chmod +x'
alias cw='chmod +w'

alias vbp='vim ~/.profile'
alias sbp='source ~/.profile'
alias vbrc='vim ~/.bashrc'
alias sbrc='source ~/.bashrc'

alias path='echo $PATH | tr : "\n"'
alias macpath='defaults read ~/.MacOSX/environment PATH | tr : "\n"'

if [[ "$(uname)" != 'Darwin' ]]; then
  alias pbcopy='xsel --clipboard --input'
  alias pbpaste='xsel --clipboard --output'
fi

alias sml='rlwrap sml'
alias ocaml='rlwrap ocaml'

if [ -x "$(which gnome-open)" ]; then
  alias o='gnome-open'
elif [ -x "$(which xdg-open)" ]; then
  alias o='xdg-open'
else
  alias o='open'
fi

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

bashrcLocal=~/.bashrc.local
[ -f "${bashrcLocal}" ] && source "${bashrcLocal}"

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

shopt -s globstar 2>/dev/null

# added by travis gem
[ -f /Users/steshaw/.travis/travis.sh ] && source /Users/steshaw/.travis/travis.sh

true
