pkgs:
let
  conditionalPkg = condition: pkg: if condition then pkg else false;
  notDarwin = pkg: conditionalPkg (!pkgs.stdenv.isDarwin) pkg;
  notARM = pkg: conditionalPkg (!pkgs.stdenv.hostPlatform.isAarch64) pkg;
  avoid = conditionalPkg false;
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
  nixfmt
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
  cabal2nix
  coreutils
  curl
  dbxcli # Defined in overlay/dbxcli.nix
  delta
  dhall
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
  git-lfs
  github-cli
  gnupg
  gopass
  gti # Humourous wrapper for git.
  hledger
  htop
  httpie
  jq
  killall
  #lastpass-cli
  licensee
  lsd
  moreutils # ts and more
  mr
  mtr
  ncdu # NCurses Disk Usage
  neofetch
  nix-direnv
  nodejs # Required for Coc.
  pandoc
  pass
  peco
  perl
  pstree
  pup
  python2
  ripgrep
  rlwrap
  rsync
  shellcheck
  speedtest-cli
  starship
  step-cli
  stow
  #terraform
  tokei
  tree
  unzip
  up
  #wakatime
  #watchman
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
  (notARM (notDarwin idris2))
  (notDarwin agda)
  rustup

  # Haskell.
  (notDarwin haskellPackages.pointfree)
  (notDarwin haskellPackages.brittany)
  cabal-install
  (notDarwin ghcid)
  (notDarwin haskell.compiler.ghc901)
  haskellPackages.fourmolu
  hlint
  stack

  # Docker
  #docker
  #docker-compose
  #docker-credential-gcr
  #amazon-ecr-credential-helper

  # Kubernetes
  #argocd
  #kind
  #kube-prompt
  #kubectl
  #kubectx
  #kubernetes-helm
  #kubespy
  #kubetail
  #kustomize

  # AWS
  #awscli2
  #python3Packages.ec2instanceconnectcli

  #
  # Google Cloud SDK.
  #
  # NOTE:
  #   Currently, it's best to get an up-to-date SDK from
  #   https://cloud.google.com/sdk/install.
  #
  (avoid pkgs.google-cloud-sdk)
]
