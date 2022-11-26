_self: super: with super; {
  steshaw = {
    brittany = haskell.lib.compose.justStaticExecutables haskellPackages.brittany;
    fourmolu = haskell.lib.compose.justStaticExecutables haskellPackages.fourmolu;
    gen-hie = haskell.lib.compose.justStaticExecutables haskellPackages.implicit-hie;
    pointfree = haskell.lib.compose.justStaticExecutables haskellPackages.pointfree;
  };
}
