{ sources ? import ./sources.nix
, nixpkgs ? sources.nixpkgs
, multiUser ? true
}:
let multiUserOverlay = _: _: {
  multiUser = multiUser;
}; in
builtins.trace "multiUser = ${(import nixpkgs {}).lib.boolToString multiUser}"
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
  } //
  # Add some packages not available for macOS ARM.
import nixpkgs {
  multiUser = multiUser;
  system = "x86_64-darwin";
  overlays = [
    (_: pkgs:
      {
        userPackagesIntel = with pkgs; if pkgs.stdenv.isDarwin then [
          idris2
        ] else [ ];
      })
  ];
}
