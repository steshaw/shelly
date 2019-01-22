#
# HT https://gist.github.com/LnL7/570349866bb69467d0caf5cb175faa74
#
# Found at
#   https://nixos.wiki/wiki/FAQ#Why_not_use_nix-env_-i_foo.3F
#
self: super:
with builtins; rec {
  userPackages = super.userPackages or {} // {

    # Packages
    inherit (self)
      cabal2nix
      git
      direnv
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
