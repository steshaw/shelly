pkgs:
let
  notDarwin = pkgs.lib.mkIf (!pkgs.stdenv.isDarwin);
  avoid = pkgs.lib.mkIf false;
  broken = avoid;
in
with pkgs; [
  #
  # Nix.
  #
  nix-prefetch-scripts
  nixfmt
  nixpkgs-fmt
  nox

  #
  # Command line utilities.
  #
  # Shells.
  fish
  zsh
  mosh
  bashInteractive_5

  bash-completion
  nix-bash-completions

  # Git.
  git
  gti # Humourous wrapper for git.
  darcs

  #
  # Security
  #
  gnupg
  gopass
  pass

  # direnv
  direnv
  nix-direnv

  # Other utilities.
  awscli2
  bat
  bind # for dig. XXX: Any smaller package?
  buildkite-cli
  cabal2nix
  (broken coreutils)
  curl
  dbxcli # Defined in overlay/dbxcli.nix
  dos2unix
  eternal-terminal
  exa
  fd
  file
  fzf
  hledger
  htop
  httpie
  jq
  killall
  lastpass-cli
  lsd
  moreutils # ts and more
  mr
  mtr
  ncdu # NCurses Disk Usage
  neofetch
  nodejs # Required for Coc.
  pandoc
  peco
  pstree
  pup
  python2
  ripgrep
  rlwrap
  rsync
  shellcheck
  speedtest-cli
  terraform
  tokei
  tree
  unzip
  up
  watchman
  wget
  youtube-dl

  gitAndTools.git-filter-repo
  nodePackages.gitmoji-cli
  pkgs.gitAndTools.hub
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
  (notDarwin pkgs.ats2)
  coq
  idris2
  agda
  go
  rustup

  # Haskell.
  haskell.compiler.ghc8102
  cabal-install
  stack
  ormolu
  ghcid
  hlint
  (broken pkgs.haskellPackages.pointfree)
  haskellPackages.brittany

  # Docker
  docker
  docker-compose
  docker-credential-gcr

  # Kubernetes
  (broken argocd)
  kind
  kube-prompt
  kubectl
  (broken kubectl-argo-rollouts)
  kubectx
  kubernetes-helm
  kubespy
  kubetail
  kustomize

  #
  # Google Cloud SDK.
  #
  # NOTE:
  #   Currently, it's best to get an up-to-date SDK from
  #   https://cloud.google.com/sdk/install.
  #
  (avoid pkgs.google-cloud-sdk)
]
