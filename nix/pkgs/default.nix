pkgs:
let
  conditionalPkg = condition: pkg: if condition then pkg else false;
  notDarwin = pkg: conditionalPkg (!pkgs.stdenv.isDarwin) pkg;
  avoid = conditionalPkg false;
  broken = avoid;
in
with pkgs; [
  #
  # Nix.
  #
  (notDarwin cachix)
  glibcLocales # Fix for locale issue.
  (notDarwin niv)
  (notDarwin nix-prefetch-scripts)
  (notDarwin nixfmt)
  nixpkgs-fmt
  nox

  bash-completion
  nix-bash-completions

  # Other utilities.
  bashInteractive_5
  bandwhich
  bat
  bind # for dig. XXX: Any smaller package?
  bottom
  buildkite-cli
  (notDarwin cabal2nix)
  coreutils
  curl
  dbxcli # Defined in overlay/dbxcli.nix
  delta
  (notDarwin dhall)
  direnv
  dos2unix
  du-dust
  dutree
  (notDarwin eternal-terminal)
  exa
  fd
  file
  fish
  fzf
  git
  github-cli
  gnupg
  gopass
  gti # Humourous wrapper for git.
  (notDarwin hledger)
  htop
  httpie
  jq
  killall
  lastpass-cli
  licensee
  lsd
  moreutils # ts and more
  mr
  mtr
  ncdu # NCurses Disk Usage
  neofetch
  nix-direnv
  nodejs # Required for Coc.
  (notDarwin pandoc)
  pass
  peco
  perl
  pstree
  pup
  python2
  ripgrep
  rlwrap
  rsync
  (notDarwin shellcheck)
  speedtest-cli
  starship
  stow
  terraform
  tokei
  tree
  unzip
  up
  wakatime
  watchman
  wget
  youtube-dl
  yq
  zsh

  gitAndTools.git-filter-repo
  nodePackages.gitmoji-cli
  pkgs.gitAndTools.lab

  # Tmux.
  tmux
  tmuxPlugins.fzf-tmux-url

  #
  # Editors
  #
  emacs
  hunspell
  hunspellDicts.en-gb-large
  neovim
  python

  #
  # Programming Languages.
  #
  (notDarwin ats2)
  (notDarwin coq)
  (notDarwin idris2)
  (notDarwin agda)
  rustup

  # Go
  go
  golangci-lint

  # Haskell.
  (notDarwin haskellPackages.pointfree)
  (notDarwin haskellPackages.brittany)
  (notDarwin cabal-install)
  (notDarwin ghcid)
  (notDarwin haskell.compiler.ghc901)
  (notDarwin haskellPackages.fourmolu)
  (notDarwin hlint)
  (notDarwin ormolu)
  (notDarwin stack)

  # Docker
  docker
  docker-compose
  docker-credential-gcr
  amazon-ecr-credential-helper

  # Kubernetes
  argocd
  kind
  kube-prompt
  kubectl
  kubectl-argo-rollouts
  kubectx
  kubernetes-helm
  kubespy
  kubetail
  kustomize

  # AWS
  awscli2
  python3Packages.ec2instanceconnectcli

  #
  # Google Cloud SDK.
  #
  # NOTE:
  #   Currently, it's best to get an up-to-date SDK from
  #   https://cloud.google.com/sdk/install.
  #
  (avoid pkgs.google-cloud-sdk)
]
