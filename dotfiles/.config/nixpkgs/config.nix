{
  packageOverrides = super: let self = super.pkgs; in {
    myHaskellEnv = self.haskell.packages.ghc802.ghcWithPackages
      (haskellPackages: with haskellPackages; [
        # libraries
        arrows async criterion lens parsec trifecta
        # tools
        cabal-install alex happy hasktags haskintex
      ]);
  };
}
