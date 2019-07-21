{
  allowUnfree = true;

  #
  # Provide packages from Nix User Repository.
  #
  # See https://github.com/nix-community/NUR.
  #
  packageOverrides = pkgs:
    let nurPath = builtins.fetchTarball {
      url = "https://github.com/nix-community/NUR/archive/a77e75c.tar.gz";
      sha256 = "sha256:11hkn5zcyxzaqlbmd8ck506j8faj0574hc25zq9nx4a27yp37cw6";
    };
    in {
      nur = import nurPath {
        inherit pkgs;
      };
  };
}
