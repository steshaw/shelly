if status is-interactive
  abbr --add --global gaa 'git add --all'
  abbr --add --global gb 'git branch --verbose --verbose'
  abbr --add --global gba 'git branch --all --verbose --verbose'
  abbr --add --global gd 'git diff'
  abbr --add --global gds 'git diff --staged'
  abbr --add --global gst 'git status'
  abbr --add --global gco 'git checkout'
  abbr --add --global gcom 'git checkout '(git-main-branch)
  abbr --add --global gp 'git push'
  abbr --add --global gpfl 'git push --force-with-lease'

  alias ls='exa --classify'
  alias l='ls --long'
  alias ll='l --all'
  alias la='ll --all'

  set CDPATH ~/Code/steshaw ~/Code/ardana ~/Code/iohk ~/Code/mlabs
end
