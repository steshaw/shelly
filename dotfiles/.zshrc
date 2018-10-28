# vim:fileencoding=utf-8:ft=zsh:foldmethod=marker

# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=~/.oh-my-zsh

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME=${ZSH_THEME:-avit}

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in ~/.oh-my-zsh/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
  git
  vi-mode
)

source $ZSH/oh-my-zsh.sh

# User configuration {{{

# Override avit theme {{{
if [[ $ZSH_THEME == avit ]]; then
  function _current_dir() {
    local _max_pwd_length="65"
    if [[ $(echo -n $PWD | wc -c) -gt ${_max_pwd_length} ]]; then
      echo "%{$fg_bold[blue]%}%-2~ ... %3~%{$reset_color%}"
    else
      echo "%{$fg_bold[blue]%}%~%{$reset_color%}"
    fi
  }
  function _user_host() {
    me='%n@%m'
    echo "%{$fg[cyan]%}$me%{$reset_color%}"
  }
  CARET='$'
  CARET2='$◀'
  PROMPT='
%{$fg[$CARETCOLOR]%}╭─$(_user_host):$(_current_dir) $(git_prompt_info) $(_ruby_version)
%{$fg[$CARETCOLOR]%}╰─${CARET}%{$resetcolor%} '
  PROMPT2='%{$fg[$CARETCOLOR]%}${CARET2}%{$reset_color%} '
  [[ $USER == 'root' ]] && CARETCOLOR='red' || CARETCOLOR='green'
  ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[yellow]%}"
  unset LSCOLORS
  unset LS_COLORS
fi
# }}}

# shellcheck source=etc/functions.sh
source $SHELLY_HOME/etc/functions.sh

sourceExists ~/.shrc

setopt noclobber

# Make up and down line-editing work like in Bash. i.e. after up/down, the
# cursor is at the beginning of the line (not the end).
bindkey -M vicmd 'j' vi-down-line-or-history
bindkey -M vicmd 'k' vi-up-line-or-history

#
# Show a mark similar to GitHub's no-newline icon.
# See https://octicons.github.com/icon/no-newline/
#
export PROMPT_EOL_MARK='%S%B🚫↵ %b%s'

# zsh somehow/strangely seems to behave differently to bash when doing `git
# diff`. This gives the old and preferred behaviour.
export PAGER='less --quit-if-one-screen --no-init'

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

function myMultilinePrompt() {
  local prompt_char="%(#:#:$)"
  local exit_status="%(?:%{$fg_bold[green]%}${prompt_char}:%{$fg_bold[red]%}${prompt_char}%s)"
  PROMPT=$"
$fg[green]╭─%{$fg_bold[blue]%}%n@%m$fg_bold[magenta]:$fg_bold[cyan]%~ \$(git_prompt_info)
$fg[green]╰─${exit_status}%{$reset_color%} "
}

# }}}
