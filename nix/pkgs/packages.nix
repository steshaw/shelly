pkgs:
let
  notDarwin = pkgs.lib.mkIf (!pkgs.stdenv.isDarwin);
  avoid = pkgs.lib.mkIf false;
  broken = avoid;
in
{
  #
  # Nix.
  #
  inherit (pkgs)
    nix-prefetch-scripts
    nixfmt
    nixpkgs-fmt
    nox
    ;

  #
  # Command line utilities.
  #
  inherit (pkgs)
    # Shells.
    fish
    zsh
    mosh
    bashInteractive_5

    bash-completion
    nix-bash-completions

    # Git.
    git
    gti# Humourous wrapper for git.
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
    awscli
    bat
    bind# for dig. XXX: Any smaller package?
    buildkite-cli
    cabal2nix
    coreutils
    curl
    dbxcli# Defined in overlay/dbxcli.nix
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
    moreutils# ts and more
    mr
    mtr
    ncdu# NCurses Disk Usage
    neofetch
    nodejs# Required for Coc.
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
    ;

  lab = pkgs.gitAndTools.lab;
  git-filter-repo = pkgs.gitAndTools.git-filter-repo;
  gitmoji = pkgs.nodePackages.gitmoji-cli;

  # Tmux.
  tmux = pkgs.tmux;
  tmux-fzf-url = pkgs.tmuxPlugins.fzf-tmux-url;

  #
  # Editors
  #
  emacs = pkgs.emacs;
  hunspell = pkgs.hunspell;
  hunspell-en-gb = pkgs.hunspellDicts.en-gb-large;
  neovim = pkgs.neovim;
  python = pkgs.python;

  #
  # Programming Languages.
  #
  ats2 = notDarwin pkgs.ats2; # Broken on macOS.
  coq = pkgs.coq;
  idris2 = pkgs.idris2;
  inherit (pkgs)
    agda
    go
    rustup
    ;

  # Haskell.
  my_ghc = pkgs.haskell.compiler.ghc8102;

  inherit (pkgs)
    cabal-install
    stack
    ormolu
    ghcid
    hlint
    ;

  pointfree = broken pkgs.haskellPackages.pointfree;
  brittany = pkgs.haskellPackages.brittany;

  # Docker
  inherit (pkgs)
    docker
    docker-compose
    docker-credential-gcr
    ;

  # Kubernetes
  inherit (pkgs)
    argocd
    kube-prompt
    kubectl
    kubectl-argo-rollouts
    kubectx
    kubernetes-helm
    kubespy
    kubetail
    kustomize
    ;

  #
  # Google Cloud SDK.
  #
  # NOTE:
  #   Currently, it's best to get an up-to-date SDK from
  #   https://cloud.google.com/sdk/install.
  #
  gcloud = avoid pkgs.google-cloud-sdk;
}
