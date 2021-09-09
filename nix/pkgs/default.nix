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
  nix-prefetch-scripts
  nixUnstable
  (notDarwin nixfmt)
  nixpkgs-fmt
  nox

  #
  # Command line utilities.
  #
  # Shells.
  fish
  zsh
  bashInteractive_5

  bash-completion
  nix-bash-completions

  # Git.
  git
  gti # Humourous wrapper for git.

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
  bat
  bind # for dig. XXX: Any smaller package?
  buildkite-cli
  (notDarwin cabal2nix)
  coreutils
  curl
  dbxcli # Defined in overlay/dbxcli.nix
  delta
  (notDarwin dhall)
  dos2unix
  (notDarwin eternal-terminal)
  exa
  fd
  file
  fzf
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
  nodejs # Required for Coc.
  (notDarwin pandoc)
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
  (notDarwin ats2)
  (notDarwin coq)
  (notDarwin idris2)
  (notDarwin agda)
  rustup

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
