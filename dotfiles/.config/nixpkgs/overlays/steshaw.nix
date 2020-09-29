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
  notDarwin = pkg: if self.stdenv.isDarwin then { } else pkg;
  broken = pkg: if false then pkg else { };
  avoid = pkg: if false then pkg else { };
  ifExistsElse = attrPath: default:
    self.lib.attrByPath attrPath self default;
  ifExists = attrPath: self.lib.attrByPath attrPath self "";
in
with builtins;
rec {
  userPackages = super.userPackages or { } // super.recurseIntoAttrs rec {

    #
    # Nix.
    #
    nixfmt = ifExists [ "nixfmt" ];
    nixpkgs-fmt = ifExists [ "nixpkgs-fmt" ];
    inherit (self)
      nix-prefetch-scripts
      nox
      ;

    #
    # Command line utilities.
    #
    bash = ifExistsElse [ "bashInteractive_5" ] self.bashInteractive;

    killall = ifExists [ "killall" ];
    lsd = ifExists [ "lsd" ];

    nushell = ifExists [ "nushell" ];
    inherit (self)
      # Shells.
      fish
      zsh

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

      # Commands.
      bat
      bind# for dig. XXX: Any smaller package?
      buildkite-cli
      cabal2nix
      coreutils
      curl
      dbxcli# Defined in overlay/dbxcli.nix
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
      hub# Defined in overlay/hub.nix
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
    lab = ifExists [ "gitAndTools" "lab" ];
    git-filter-repo = self.gitAndTools.git-filter-repo;
    gitmoji = ifExists [ "gitmoji-cli" ];

    # Tmux.
    tmux = self.tmux;
    tmux-fzf-url = ifExists [ "tmuxPlugins" "fzf-tmux-url" ];

    #
    # Editors
    #
    emacs = self.emacs;
    hunspell = super.hunspell;
    hunspell-en-gb = ifExists [ "hunspellDicts" "en-gb-large" ];
    neovim = self.neovim;
    python = self.python;

    #
    # Programming Languages.
    #
    ats2 = notDarwin self.ats2; # Broken on macOS.
    coq = avoid self.coq;
    idris2 = ifExists [ "idris2" ];
    inherit (self)
      agda
      go
      rustup
      ;

    # Haskell.
    #my_GHC = ifExistsElse "ghc8102" (ifExists "ghc884" (ifExist "ghc self.haskell.compiler.ghc884;
    my_ghc =
      if builtins.hasAttr "ghc8102" (builtins.getAttr "compiler" (builtins.getAttr "haskell" self))
      then self.haskell.compiler.ghc8102
      else "";
    /*
    myGHC = if (builtins.tryEval
       (builtins.deepSeq self["haskell"]["compiler"]["ghc8102"])).success
       then self.haskell.compiler.ghc8102 else "";
       */

    inherit (self)
      cabal-install
      stack
      ;
    pointfree = broken self.haskellPackages.pointfree;
    brittany = avoid self.haskellPackages.brittany;
    ghcid = if builtins.hasAttr "ghcid" self then self.ghcid else "";
    ormolu = ifExists [ "ormolu" ];
    inherit (self)
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
      nix# Don't enable this on multi-user.
      cacert;
  };
}
