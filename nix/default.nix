{ sources ? import ./sources.nix
, nixpkgs ? sources.nixpkgs
, multiUser ? true
}:
let multiUserOverlay = _: _: {
  multiUser = multiUser;
}; in
let
  intelPkgs =
    import nixpkgs {
      multiUser = multiUser;
      system = "x86_64-darwin";
    };
  idrisOverlay = _: _: {
    idris2Intel = intelPkgs.idris2;
  };
in
builtins.trace "multiUser = ${(import nixpkgs {}).lib.boolToString multiUser}"
  import
  nixpkgs
{
  overlays =
    let path = ./overlays; in
    with builtins;
    [ multiUserOverlay idrisOverlay ] ++
    map (n: import (path + ("/" + n)))
      (filter
        (n: match ".*\\.nix" n != null ||
        pathExists (path + ("/" + n + "/default.nix")))
        (attrNames (readDir path)));
  config = { };
}
