#
# This default.nix is for convenient installation of adhoc
# packages at the same nixpkgs pin as other user packages.
# e.g.
#
#   nix-env -f ~/Code/steshaw/shelly --install --attr git-crypt
#

{ sources ? import ./nix/sources.nix
, nixpkgs ? sources.nixpkgs
, system ? builtins.currentSystem
}:
import nixpkgs {
  inherit system;
  overlays = [ ];
  config = { };
}
