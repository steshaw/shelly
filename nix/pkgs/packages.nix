self: super:
let
  notDarwin = super.lib.mkIf (!super.stdenv.isDarwin);
  broken = super.lib.mkIf false;
in
rec {
  #userPackages = super.userPackages or { } // super.recurseIntoAttrs rec {

    #
    # Nix.
    #
    inherit (super)
      nix-prefetch-scripts
      nixfmt
      nixpkgs-fmt
      nox
      ;

    #
    # Command line utilities.
    #
    bash = super.bashInteractive_5;

    killall = super.killall;
    lsd = super.lsd;

    inherit (super)
      # Shells.
      fish
      zsh
      mosh

      bash-completion
      nix-bash-completions

      # Git.
      git
      gti# Humourous wrapper for git.
      darcs

      #
      # Security
      #
      gnupg22
      gopass
      pass

      direnv
      nix-direnv

      # Commands.
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
      lastpass-cli
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
      watchman
      wget
      youtube-dl
      ;

    lab = super.gitAndTools.lab;
    git-filter-repo = super.gitAndTools.git-filter-repo;
    gitmoji = super.nodePackages.gitmoji-cli;

    # Tmux.
    tmux = super.tmux;
    tmux-fzf-url = super.tmuxPlugins.fzf-tmux-url;

    #
    # Editors
    #
    emacs = super.emacs;
    hunspell = super.hunspell;
    hunspell-en-gb = super.hunspellDicts.en-gb-large;
    neovim = super.neovim;
    python = super.python;

    #
    # Programming Languages.
    #
    ats2 = notDarwin super.ats2; # Broken on macOS.
    coq = super.coq;
    idris2 = super.idris2;
    inherit (super)
      agda
      go
      rustup
      ;

    # Haskell.
    my_ghc = super.haskell.compiler.ghc8102;

    inherit (super)
      cabal-install
      stack
      ormolu
      ghcid
      hlint
      ;
    pointfree = broken super.haskellPackages.pointfree;
    brittany = super.haskellPackages.brittany;

    # Kubernetes
    inherit (super)
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
    /*
    inherit (super)
      google-cloud-sdk

      docker-credential-gcr
      kubectl
    ;
    */

    #
    # Default packages.
    #
    # WARNING: Do not remove!
    #
    /*
    inherit (super)
      nix# Don't enable this on multi-user.
      cacert;
      */
  #};
}
