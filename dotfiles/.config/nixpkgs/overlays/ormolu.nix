# This overlay adds Ormolu straight from GitHub.
self: super:

let source = builtins.fetchGit {
#      owner = "tweag";
#      repo = "ormolu";
      url = https://github.com/tweag/ormolu;
      rev = "a4e97da37dda5bbcbe410c7300a1b63205402693"; # update as necessary
#      sha256 = "0qrxfk62ww6b60ha9sqcgl4nb2n5fhf66a65wszjngwkybwlzmrv"; # as well
    };
    ormolu = import source { pkgs = self; };
in {
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      "${ormolu.ormoluCompiler}" = super.haskell.packages.${ormolu.ormoluCompiler}.override {
        overrides = ormolu.ormoluOverlay;
      };
    };
  };
}
