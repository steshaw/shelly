self: super: {
  brittany = self.haskell.lib.enableSeparateBinOutput self.haskellPackages.brittany;
  ghcid = self.haskell.lib.enableSeparateBinOutput self.haskellPackages.ghcid;
  hindent = self.haskell.lib.enableSeparateBinOutput self.haskellPackages.hindent;
  hlint = self.haskell.lib.enableSeparateBinOutput self.haskellPackages.hlint;
  pointfree = self.haskell.lib.enableSeparateBinOutput self.haskellPackages.pointfree;
  ormolu = self.haskell.lib.enableSeparateBinOutput self.haskellPackages.ormolu;
}
