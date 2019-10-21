{
  allowUnfree = true;

  #
  # Provide packages from Nix User Repository.
  #
  # See https://github.com/nix-community/NUR.
  #
  packageOverrides = pkgs:
    let
      old = true;
      nurPkgs = if old then builtins.fetchTarball {
        url = "https://github.com/nix-community/NUR/archive/a77e75c.tar.gz";
        sha256 = "sha256:11hkn5zcyxzaqlbmd8ck506j8faj0574hc25zq9nx4a27yp37cw6";
      } else builtins.fetchGit {
        url = "https://github.com/nix-community/NUR";
        rev = "6b7e611ad65693f954112f7d346bc6b4e779ea08";
      };
    in {
      nur = import nurPkgs {
        inherit pkgs;
      };
  };

}
