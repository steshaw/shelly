let pkgs = import (builtins.fetchGit {
  name = "nixos-19.09-20191216-021504";
  url = https://github.com/NixOS/nixpkgs-channels;
  ref = "nixos-19.09";
  # `git ls-remote https://github.com/NixOS/nixpkgs-channels nixos-19.09`
#  rev = "d85e435b7bded2596d7b201bcd938c94d8a921c1";
}) {};
in
self: super:
{
  eternal-terminal-5 = pkgs.eternal-terminal;
}
