let
  sources = import ../nix/sources.nix;
  nvim = sources.neovim-nightly-overlay;
in
(import (builtins.fetchTarball {
  url = nvim.url;
  sha256 = nvim.sha256;
}))
