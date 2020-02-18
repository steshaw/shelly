# This overlay adds Ormolu straight from GitHub.
self: super:
let
  source = builtins.fetchGit {
    url = "https://github.com/tweag/ormolu";
    rev = "231b93c5d4718da20b779e2ae6a81a9e1285f1d0";
  };
  ormolu = import source { pkgs = self; };
  # Seems that ormolu is now in nixpkgs, so disable.
  enable = true;
in
if enable
then {
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      "${ormolu.ormoluCompiler}" =
        super.haskell.packages.${ormolu.ormoluCompiler}.override {
          overrides = ormolu.ormoluOverlay;
        };
    };
  };
} else {}
