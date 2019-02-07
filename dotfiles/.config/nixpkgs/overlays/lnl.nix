#
# HT https://gist.github.com/LnL7/570349866bb69467d0caf5cb175faa74
#
# Found at
#   https://nixos.wiki/wiki/FAQ#Why_not_use_nix-env_-i_foo.3F
#
self: super:
with builtins; rec {
/*
  # Trying to get equivalence to compile...
  haskellPackages = super.haskellPackages.extend (self: super_: {
    equivalence = super_.equivalence.overrideDerivation (attrs: {
      doCheck = false;
      src = super.fetchgit {
        url = "https://github.com/pa-ba/equivalence";
        sha256 = "1dr78i7dsydbi7gxr4864an907xydvmybyya5f5lbgdbv2cy1g7i";
        rev = "b194d4c4b3edd86d786139fdce99a4d19b9e4b57";
      };
    });
  });
*/
  userPackages = super.userPackages or {} // {

    # Packages
#    Agda = super.haskellPackages.Agda; # Agda is currently broken in nixpkgs
#    because of equivalence.
    # XXX: Should I call this `my_vim` instead?
    vim = (super.vim_configurable.override {
      guiSupport = "no";
      darwinSupport = super.stdenv.isDarwin;
      python = super.python3;
    }).overrideAttrs (prevAttrs: {
      name = "my-vim-${prevAttrs.version}";
    });
    inherit (self)
      bashInteractive_5
      cabal2nix
      direnv
      gist
      git # Better to use Homebrew git? Better macOS Keychain (i.e. UseKeychain).
      gnupg22
      htop
      httpie
      jq
      mr
      neofetch
      neovim
      ripgrep
      rlwrap
      shellcheck
      stack
      tmux
      tree
      watchman
      youtube-dl
      zsh
      ;

    # Default packages.
    # WARNING: Do not remove!
    inherit (self)
      nix # Don't enable this on multi-user.
      cacert;

    nix-rebuild = super.writeScriptBin "nix-rebuild" ''
      #!${super.stdenv.shell}
      if ! command -v nix-env &>/dev/null; then
        echo "warning: nix-env was not found in PATH, add nix to userPackages" >&2
        PATH=${self.nix}/bin:$PATH
      fi
      exec nix-env -f '<nixpkgs>' -r -iA userPackages "$@"
    '';
  };
}
