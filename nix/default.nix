{ sources ? import ./nix/sources.nix }:
with
{
  overlay = _: pkgs:
    {
      niv = (import sources.niv { }).niv;
    };
};
let
  overlays =
    let path = ./overlays; in
    with builtins;
    map (n: import (path + ("/" + n)))
      (filter
        (n: match ".*\\.nix" n != null ||
          pathExists (path + ("/" + n + "/default.nix")))
        (attrNames (readDir path)));
in
import sources.nixpkgs
{
  overlays = [ overlay ] ++ overlays;
  config = { };
}
