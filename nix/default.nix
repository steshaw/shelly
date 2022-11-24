{ sources ? import ./sources.nix
, nixpkgs ? sources.nixpkgs
, multiUser ? true
}:
let
  pkgs = import nixpkgs { };
  darwinIntelPkgs = import nixpkgs {
    system = "x86_64-darwin";
  };
  multiUserOverlay = _: _: {
    multiUser = multiUser;
    darwinIntelPkgs = darwinIntelPkgs;
  };
in
builtins.trace "multiUser = ${pkgs.lib.boolToString multiUser}"
  import
  nixpkgs
{
  overlays =
    let path = ./overlays; in
    with builtins;
    [ multiUserOverlay ] ++
    map (n: import (path + ("/" + n)))
      (filter
        (n: match ".*\\.nix" n != null ||
        pathExists (path + ("/" + n + "/default.nix")))
        (attrNames (readDir path)));
  config = { };
}
