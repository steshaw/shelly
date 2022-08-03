# Usage:
#
#   Find version:
#
#     $ nix-instantiate --eval --expr '(import ./pkg-versions.nix {}).pandoc'
#     { "1.13.1" = <CODE>; "1.16.0.2" = <CODE>; "1.17.1" = <CODE>; "1.19.2.1" = <CODE>; "1.19.2.4" = <CODE>; "2.0.6" = <CODE>; "2.2.1" = <CODE>; "2.7.1" = <CODE>; }
#     $ nix eval '(builtins.attrNames (import ./pkg-versions.nix {}).pandoc)'
#     [ "1.13.1" "1.16.0.2" "1.17.1" "1.19.2.1" "1.19.2.4" "2.0.6" "2.2.1" "2.7.1" ]
#     $ nix-instantiate --eval --expr 'builtins.attrNames (import ./pkg-versions.nix {}).pandoc'
#     [ "1.13.1" "1.16.0.2" "1.17.1" "1.19.2.1" "1.19.2.4" "2.0.6" "2.2.1" "2.7.1" ]
#
#   Run arbitrary version:
#
#     $ LANG=C $(nix-build --no-out-link --expr '(import ./pkg-versions.nix {}).pandoc."1.16.0.2".pkg')/bin/pandoc --version
#     pandoc 1.16.0.2
#     ...
#     $ nix run '(import ./pkg-versions.nix {}).pandoc."1.16.0.2".pkg' -u LANG -c pandoc --version
#     pandoc 1.16.0.2
#     ...
#
#   Find channel for version:
#
#     $ nix eval '(import ./pkg-versions.nix {}).pandoc."1.16.0.2".channel'
#     "16.03"
#     $ nix-instantiate --eval --expr '(import ./pkg-versions.nix {}).pandoc."1.16.0.2".channel'
#     "16.03"
#
#   Run a version from a channel:
#
#     $ nix run -f channel:nixos-16.03 pandoc --command pandoc --version
#
# HT
#   http://matthewbauer.us/blog/all-the-versions.html
#   https://gist.github.com/matthewbauer/7c57f8fb69705bb8da9741bf4b9a7e64
#

let defaultChannels = map (version: "channel:nixos-${version}") [
  "20.09"
  "20.03"
  "19.09"
  "19.03"
  "18.09"
  "18.03"
  "17.09"
  "17.03"
  "16.09"
  "16.03"
  "15.09"
  "14.12"
  "14.04"
  "13.10"
] ++ ["channel:nixos-unstable"]; in
{
  nixpkgs ? (import <nixpkgs> {}),
  channels ? defaultChannels,
  attrs ? builtins.attrNames nixpkgs,
  system ? builtins.currentSystem, args ? { inherit system; } }:
let
  getSet = channel:
    let tarball = builtins.fetchTarball channel;
    in (import tarball args).pkgs;

  pkg2version = name: channel:
    let
      pkgs = getSet channel;
      pkg = pkgs.${name};
      version = (builtins.parseDrvName pkg.name).version;
    in if builtins.hasAttr name pkgs && pkg ? name then {
      name = version;
      value = {
        channel = channel;
        pkg = pkg;
        rev = nixpkgs.lib.fileContents ((builtins.fetchTarball "channel:nixos-13.10") + "/nixpkgs/.git-revision");
      };
    } else
      null;

  attr2version = name:
    let
      pkgs = map (pkg2version name) channels;
      nonNullPkgs = builtins.filter (x: x != null) pkgs;
    in {
      name = name;
      value = builtins.listToAttrs nonNullPkgs;
    };

in builtins.listToAttrs (map attr2version attrs)
