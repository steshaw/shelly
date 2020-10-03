/*
    (builtins.fetchGit {
      url = "https://github.com/NixOS/nixpkgs";
      rev = "dfd2eeabd6e1be22676dac26854a2de21c3d4c87";
    })
*/
let
  pkgs = builtins.fetchTarball
    https://github.com/nixos/nixpkgs/archive/c59ea8b8a0e7f927e7291c14ea6cd1bd3a16ff38.tar.gz;
    overlays =
      let path = ./overlays; in with builtins;
      map (n: import (path + ("/" + n)))
          (filter (n: match ".*\\.nix" n != null ||
                      pathExists (path + ("/" + n + "/default.nix")))
                  (attrNames (readDir path)));
in
import pkgs {
  overlays = overlays;
}
