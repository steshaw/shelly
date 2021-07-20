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
  niv
  nix-prefetch-scripts
  nixfmt
  nixpkgs-fmt
  nox
  glibcLocales # Fix for locale issue.

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
  coreutils
  curl
  dbxcli # Defined in overlay/dbxcli.nix
  delta
  dhall
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
  licensee
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
  wakatime
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
  neovim-nightly
  python

  #
  # Programming Languages.
  #
  (notDarwin pkgs.ats2)
  coq
  idris2
  (broken agda)
  rustup

  # Haskell.
  haskell.compiler.ghc901
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

  #
  # Google Cloud SDK.
  #
  # NOTE:
  #   Currently, it's best to get an up-to-date SDK from
  #   https://cloud.google.com/sdk/install.
  #
  (avoid pkgs.google-cloud-sdk)
]
