self: super:
{}
/*
self: super:
let
  source = super.fetchFromGitHub {
    owner = "tweag";
    repo = "ormolu";
    rev = "de279d80122b287374d4ed87c7b630db1f157642"; # 0.1.2.0
    sha256 = "0qrxfk62ww6b60ha9sqcgl4nb2n5fhf66a65wszjngwkybwlzmrv"; # as well
  };
  ormolu = import source { pkgs = self; };
in
{
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      "${ormolu.ormoluCompiler}" = super.haskell.packages.${ormolu.ormoluCompiler}.override {
        overrides = ormolu.ormoluOverlay;
      };
    };
  };
}
*/
