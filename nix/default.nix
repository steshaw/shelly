{ sources ? import ./sources.nix
, nixpkgs ? sources.nixpkgs
, multiUser ? true
}:
let
  overlay = _: pkgs: {
    niv = (import sources.niv { }).niv;
    multiUser = multiUser;
  };
  overlays =
    let path = ./overlays; in
    with builtins;
    map (n: import (path + ("/" + n)))
      (filter
        (n: match ".*\\.nix" n != null ||
          pathExists (path + ("/" + n + "/default.nix")))
        (attrNames (readDir path)));
in
builtins.trace "multiUser = ${(import nixpkgs {}).lib.boolToString multiUser}"
import nixpkgs {
  overlays = [ overlay ] ++ overlays;
  config = { };
}
