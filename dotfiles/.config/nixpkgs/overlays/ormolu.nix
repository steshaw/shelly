# This overlay adds Ormolu straight from GitHub.
self: super:
let
  source = builtins.fetchGit {
    url = "https://github.com/tweag/ormolu";
    #
    # Find the latest rev with
    #
    #   git ls-remote https://github.com/tweag/ormolu master
    #
    rev = "3fbc130a0962ab236d87711beae1895d84a68362";
  };
  ormolu = import source { pkgs = self; };
  # Seems that ormolu is now in nixpkgs, so disable.
  enable = false;
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
