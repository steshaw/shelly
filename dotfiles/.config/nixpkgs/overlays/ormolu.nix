# This overlay adds Ormolu straight from GitHub.
self: super:

let
  source = builtins.fetchGit {
    url = "https://github.com/tweag/ormolu";
    rev = "43c6f4563428e871d760312ed1e414a51224c074"; # update as necessary
  };
  ormolu = import source { pkgs = self; };
in {
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      "${ormolu.ormoluCompiler}" =
        super.haskell.packages.${ormolu.ormoluCompiler}.override {
          overrides = ormolu.ormoluOverlay;
        };
    };
  };
}
