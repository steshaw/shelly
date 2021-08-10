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
  cachix
  glibcLocales # Fix for locale issue.
  niv
  nix-prefetch-scripts
  nix
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
  yq

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
  (if false then neovim-nightly else neovim)
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
  (broken pkgs.haskellPackages.pointfree)
  (notDarwin pkgs.haskellPackages.brittany)
  cabal-install
  ghcid
  haskell.compiler.ghc901
  haskellPackages.fourmolu
  hlint
  ormolu
  stack

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
