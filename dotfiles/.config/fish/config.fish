if status is-interactive
  alias ls='exa --classify'
  alias l='ls --long'
  alias ll='l --all'
  alias la='ll --all'

  abbr --add --global cx 'chmod +x'
  abbr --add --global cw 'chmod +w'

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
  abbr --add --global gcane 'git commit --amend --no-edit'

  abbr --add --global grep 'grep --directories=skip'

  set CDPATH ~/Code/steshaw ~/Code/ardana ~/Code/iohk ~/Code/mlabs ~/Code

  # vi-style search as in Bash
  # Cribbed from https://github.com/fish-shell/fish-shell/issues/2271#issuecomment-352210807
  function reverse_history_search
    history | fzf --no-sort | read -l command
    if test $command
      # FIXME: This doesn't display the new command without typing 'l'
      commandline -rb $command
    end
  end

  bind -M default / reverse_history_search
end
