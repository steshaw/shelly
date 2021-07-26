# Renaming the default Git branch from master to main.
function gbmm
  git branch -m master main
  and git fetch origin
  and git branch --set-upstream-to=origin/main main
  and git remote set-head origin --auto
end
