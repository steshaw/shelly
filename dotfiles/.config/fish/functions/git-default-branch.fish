function git-default-branch
  if command git show-ref -q --verify refs/heads/master
    echo master
  else
    echo main
  end
end
