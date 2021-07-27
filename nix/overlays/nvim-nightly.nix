let
  sources = import ../sources.nix;
  nvim = sources.neovim-nightly-overlay;
in
(import (builtins.fetchTarball {
  url = nvim.url;
  sha256 = nvim.sha256;
}))
