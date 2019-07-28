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
  notDarwin = pkg: if super.stdenv.isDarwin then super.zsh else pkg;
in
with builtins; rec {
  userPackages = super.userPackages or {} // super.recurseIntoAttrs rec {

    #
    # Nix.
    #
    inherit (self)
      nix-prefetch-scripts
    ;

    #
    # Something like build-essential.
    #
    inherit (self)
      gcc
      gnumake
      pkgconfig
      python
    ;

    #
    # CLIs.
    #
    bash = super.bashInteractive_5;
    inherit (self)
      # Shells.
      fish
      zsh

      # VCS system.
      bazaar
      darcs
      git
      mercurial
      pijul

      # Security related.
      gnupg22
      gopass
      pass

      # Commands.
      bat
      bind # for dig. XXX: Any smaller package?
      cabal2nix
      direnv
      dos2unix
      fd
      fzf
      gist
      hledger
      htop
      httpie
      hub # Defined in overlay/hub.nix
      jq
      killall
      mr
      neofetch
      pandoc
      ripgrep
      rlwrap
      shellcheck
      tree
      unzip
      watchman
      youtube-dl
    ;

    # Tmux.
    tmux = self.tmux;
    tmux-fzf-url = self.tmuxPlugins.fzf-tmux-url;

    #
    # Editors
    #
    emacs = self.emacs;
    neovim = self.neovim;

    # Vim with Python3 for vim-orgmode support.
    vim_ =
      if true # Disable vim for now. It doesn't compile on Darwin anyhow.
      then self.zsh # For want of an "empty" package.
      else ((self.vim_configurable.override {
      guiSupport = "no";
      darwinSupport = super.stdenv.isDarwin;
      python = self.python3;
    }).overrideAttrs (prevAttrs: {
      name = "my-vim-${prevAttrs.version}";
    }));

    #
    # Programming Languages.
    #
    inherit (self)
      ocaml
      rustup
    ;

    # Dependently typed PLs.
    agda = notDarwin self.haskellPackages.Agda;
    ats2 = notDarwin self.ats2;
    idris1 = notDarwin self.idris;
    inherit (self)
      coq
    ;

    # Haskell.
    ghc865 = self.haskell.compiler.ghc865;
    stack = notDarwin self.haskellPackages.stack;
    brittany = self.haskellPackages.brittany;
    hindent = notDarwin self.haskellPackages.hindent;
    hlint = notDarwin self.haskellPackages.hlint;

    #
    # Google Cloud Platform.
    #
    inherit (self)
      google-cloud-sdk

      docker-credential-gcr
      kubectl
    ;

    # Yarn for gitmoji-cli.
    nodejs = self.nodejs;
    yarn = self.yarn;
/*
    yarn_ = (self.yarn.override {
      nodejs = self.nodejs;
    });
*/

    # --------------------------------------------------------------------------
    # X related
    # --------------------------------------------------------------------------

    # Fonts.
    inherit (self)
      fira-code
      source-code-pro
    ;

    inherit (self)
      xsel
    ;
    inherit (self.xorg)
      xev
    ;
    randr = self.xorg.xrandr;

    # KDE.
    gwenview = notDarwin self.gwenview; # image viewer
    spectacle = notDarwin self.kdeApplications.spectacle;

    # Would like to remote in from laptop...
    nomachine-client = notDarwin self.nomachine-client;
    teamviewer = notDarwin self.teamviewer;
    tigervnc = notDarwin self.tigervnc;
    x11vnc = notDarwin self.x11vnc;
    slack-dark = if true then zsh else notDarwin self.slack-dark; # Slack disabled — use Firefox.

    # https://github.com/KSmanis/kwin-move-window-to-center
    # but let's have xmonad+KDE.

    #
    # Default packages.
    #
    # WARNING: Do not remove!
    #
    inherit (self)
      nix # Don't enable this on multi-user.
      cacert;

    #
    # nix-rebuild script.
    #
    nix-rebuild = super.writeScriptBin "nix-rebuild" ''
      #!${super.stdenv.shell}
      if ! command -v nix-env &>/dev/null; then
        echo "warning: nix-env was not found in PATH, add nix to userPackages" >&2
        PATH=${self.nix}/bin:$PATH
      fi
      nix-env -f '<nixpkgs>' -r -iA userPackages "$@"

      #
      # Fun and silly commit messages.
      #
      # FIXME: Mutation, hah!
      #
      yarn --silent global add gitmoji-cli
    '';

    #
    # FIXME: Fix below which provides a diff but breaks with use of `super.recurseIntoAttrs` above.
    #        The `super.recurseIntoAttrs` allows you to find userPackages when doing `nix search`.
/*
    nix-rebuild = super.writeScriptBin "nix-rebuild" ''
      #!${super.stdenv.shell}

#      #!${super.stdenv.shell}
#      if ! command -v nix-env &>/dev/null; then
#        echo "warning: nix-env was not found in PATH, add nix to userPackages" >&2
#        PATH=${self.nix}/bin:$PATH
#      fi
#      exec nix-env -f '<nixpkgs>' -r -iA userPackages "$@"

      #!${super.stdenv.shell}
      set -e
      if ! command -v nix-env &>/dev/null; then
        echo "warning: nix-env was not found in PATH, add nix to userPackages" >&2
        PATH=${self.nix}/bin:$PATH
      fi
      IFS=- read -r _ oldGen _ <<<"$(readlink "$(readlink ~/.nix-profile)")"
      oldVersions=$(readlink ~/.nix-profile/package_versions || echo "/dev/null")
      nix-env -f '<nixpkgs>' -r -iA userPackages "$@"
      IFS=- read -r _ newGen _ <<<"$(readlink "$(readlink ~/.nix-profile)")"
      ${self.diffutils}/bin/diff --color -u --label "generation $oldGen" $oldVersions \
        --label "generation $newGen" ~/.nix-profile/package_versions \
        || true
    '';

    packageVersions =
      let
        versions = super.lib.attrsets.mapAttrsToList (_: pkg: pkg.name) self.userPackages;
        versionText = super.lib.strings.concatMapStrings (s: s+"\n") versions;
      in
      super.writeTextDir "package_versions" versionText;
*/
  };
}
