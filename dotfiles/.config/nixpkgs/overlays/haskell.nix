self: super: {
  brittany =
    self.haskell.lib.enableSeparateBinOutput self.haskellPackages.brittany;
  ghcid = self.haskell.lib.enableSeparateBinOutput self.haskellPackages.ghcid;
  hindent =
    self.haskell.lib.enableSeparateBinOutput self.haskellPackages.hindent;
  hlint = self.haskell.lib.enableSeparateBinOutput self.haskellPackages.hlint;
  pointfree =
    self.haskell.lib.enableSeparateBinOutput self.haskellPackages.pointfree;


  # XXX: Seems to prevent `nix-env -qa` when used. With the following error:
  #      error: attribute 'ormolu' missing, at /home/steshaw/.config/nixpkgs/overlays/haskell.nix:10:53
  ormolu = if false then self.haskell.lib.enableSeparateBinOutput self.haskellPackages.ormolu else {};
}
