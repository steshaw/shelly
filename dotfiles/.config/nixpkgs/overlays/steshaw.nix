#
# HT https://gist.github.com/LnL7/570349866bb69467d0caf5cb175faa74
#
# Initially install with
#  $ nix-env -f '<nixpkgs>' -r -iA userPackages
#
# Afterwards, edit the file and run:
#
#  $ nix-rebuild
#
# See https://gist.github.com/LnL7/570349866bb69467d0caf5cb175faa74#gistcomment-2598159
#
# See also:
#  https://gist.github.com/Widdershin/de023630617c405b033245ced16221f2
#
# Found at
#   https://nixos.wiki/wiki/FAQ#Why_not_use_nix-env_-i_foo.3F
#
self: super:
let
  notDarwin = pkg: if self.stdenv.isDarwin then {} else pkg;
  broken = pkg: if false then pkg else {};
  avoid = pkg: if false then pkg else {};
in
with builtins;
rec {
  userPackages = super.userPackages or {} // super.recurseIntoAttrs rec {

    #
    # Nix.
    #
    inherit (self)
      nix-prefetch-scripts
      nixfmt
      nixpkgs-fmt
      nox
    ;

    #
    # Command line utilities.
    #
    bash = self.bashInteractive_5;
    inherit (self)
      # Shells.
      fish
      nushell
      zsh

      bash-completion
      nix-bash-completions

      # Git.
      git
      gti # Humourous wrapper for git.
      darcs

      #
      # Security
      #
      gnupg22
      gopass
      pass

      # Commands.
      bat
      bind # for dig. XXX: Any smaller package?
      buildkite-cli
      cabal2nix
      coreutils
      curl
      dbxcli # Defined in overlay/dbxcli.nix
      direnv
      dos2unix
      eternal-terminal
      exa
      fd
      file
      fzf
      hledger
      htop
      httpie
      hub # Defined in overlay/hub.nix
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
      watchman
      wget
      youtube-dl
    ;
    lab = self.gitAndTools.lab;
    gitmoji = self.nodePackages.gitmoji-cli;

    # Tmux.
    tmux = self.tmux;
    tmux-fzf-url = self.tmuxPlugins.fzf-tmux-url;

    #
    # Editors
    #
    emacs = self.emacs;
    hunspell = super.hunspell;
    hunspell-en-gb = super.hunspellDicts.en-gb-large;
    neovim = self.neovim;
    python = self.python;

    #
    # Programming Languages.
    #
    ats2 = notDarwin self.ats2; # Broken on macOS.
    coq = avoid self.coq;
    inherit (self)
      agda
      go
      idris2
      rustup
    ;

    # Haskell.
    ghc844 = self.haskell.compiler.ghc884;
    inherit (self)
      cabal-install
      stack
    ;
    pointfree = broken self.haskellPackages.pointfree;
    brittany = avoid self.haskellPackages.brittany;
    inherit (self)

      ghcid
      ormolu
      hlint;

    #
    # Google Cloud SDK.
    #
    # NOTE:
    #   Currently, it's best to get an up-to-date SDK from
    #   https://cloud.google.com/sdk/install.
    #
/*
    inherit (self)
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
    inherit (self)
      nix # Don't enable this on multi-user.
      cacert;
  };
}
