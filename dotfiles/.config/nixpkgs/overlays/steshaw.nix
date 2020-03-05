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
in
with builtins;
rec {
  userPackages = super.userPackages or {} // super.recurseIntoAttrs rec {

    # HIE. Disabled as it is installed in systemPackages.
    hie-ghc865 = let enable_hie = false; in if enable_hie then (
      let all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
      in
      all-hies.selection { selector = p: { inherit (p) ghc865; }; }) else {};

    # ghcide. Disabled as we are currently using HIE.
    ghcide-ghc865 = let enable_ghcide = false; in if enable_ghcide then (import (builtins.fetchTarball
    "https://github.com/cachix/ghcide-nix/tarball/master")
    {}).ghcide-ghc865 else {};

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

      # SCM systems.
      bazaar
      darcs
      git
      gti # Humourous wrapper for git.
      mercurial
      pijul

      #
      # Security
      #
      gnupg22
      gopass
      pass
      pinentry

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
      eternal-terminal-5 # from ./eternal-terminal-5.nix
      exa
      fd
      file
      fzf
      gist
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
      neofetch
      nodejs # Required for Coc.
      pandoc
      peco
      pup
      python2
      ripgrep
      rlwrap
      rsync
      shellcheck
      speedtest-cli
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

    # Vim with Python3 for vim-orgmode support.
    vim_ = if false then null else (
      let enableVim = true;
      in
      if enableVim
      then ((self.vim_configurable.override {
        guiSupport = "no";
        darwinSupport = self.stdenv.isDarwin;
        python = self.python3;
      }).overrideAttrs (prevAttrs: {
        name = "my-vim-${prevAttrs.version}";
      }))
      else {}
    );
    # Required for Vim's Neovim compatibility.
    python3 = self.python3.withPackages (
      ps: with ps; [ pynvim numpy toolz ]
    );

    #
    # Programming Languages.
    #
    agda = notDarwin self.haskellPackages.Agda; # Broken on macOS.
    ats2 = notDarwin self.ats2; # Broken on macOS.
    coq = if false then self.coq else {};
    idris = if true then self.idris else {};
    rustup = if true then self.rustup else {};

    # Haskell.
    ghc865 = if true then "" else self.haskell.compiler.ghc865;
    inherit (self)
      cabal-install
      stack
      hindent # From haskell overlay.
      pointfree # From haskell overlay.
    ;
    ghcid = self.haskellPackages.ghcid;
    ormolu = self.haskellPackages.ormolu;
    brittany = self.haskellPackages.brittany;
    hlint = self.haskellPackages.hlint;

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
