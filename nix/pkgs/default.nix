pkgs:
let
  conditionalPkg = condition: pkg: if condition then pkg else false;
  notDarwin = pkg: conditionalPkg (!pkgs.stdenv.isDarwin) pkg;
  avoid = conditionalPkg false;
in
with pkgs; [
  #
  # Nix.
  #
  cachix
  glibcLocales # Fix for locale issue.
  niv
  nix-prefetch-scripts
  nixfmt-classic
  nixpkgs-fmt
  nox

  bash-completion
  nix-bash-completions

  # Emacs
  # Disabling native compilation as workaround for
  # https://github.com/NixOS/nixpkgs/issues/395169 on macOS.
  ((emacs29.override { withNativeCompilation = false; }).pkgs.withPackages (epkgs: with epkgs; [
    vterm
  ]))

  # Other utilities.
  bandwhich
  bashInteractive
  bat
  bind # for dig. XXX: Any smaller package?
  bottom
  buildkite-cli
  (notDarwin cabal2nix)
  cmake
  coreutils
  curl
  delta
  dhall
  direnv
  dos2unix
  du-dust
  dutree
  eternal-terminal
  eza
  fastfetch
  fd
  file
  fish
  fzf
  git
  git-credential-manager
  git-lfs
  github-cli
  gnupg
  gopass
  gti # Humourous wrapper for git.
  hledger
  htop
  httpie
  hunspell
  hunspellDicts.en-gb-large
  jq
  killall
  libtool
  licensee
  lsd
  moreutils # ts and more
  mr
  mtr
  (notDarwin ncdu) # NCurses Disk Usage
  neovim
  #nix-direnv
  nodejs # Required for Coc.
  pandoc
  pass
  peco
  perl
  pstree
  pup
  python3
  ripgrep
  rlwrap
  rsync
  shellcheck
  speedtest-cli
  starship
  step-cli
  stow
  tmate
  tmux
  tmuxPlugins.fzf-tmux-url
  tokei
  tree
  unzip
  up
  wget
  world
  yt-dlp
  yq
  zsh
  zstd

  gitAndTools.git-filter-repo
  nodePackages.gitmoji-cli
  pkgs.gitAndTools.lab

  # Docker
  #docker
  #docker-compose
  #docker-credential-gcr
  #amazon-ecr-credential-helper
  docker-credential-helpers

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
