#
# This flake.nix is for convenient installation of adhoc
# packages at the same nixpkgs pin as other user packages.
# e.g.
#
#   nix profile install ~/Code/steshaw/shelly#git-crypt
#

{
  description = "A flake for dynamically accessing all packages from default.nix on multiple platforms";

  outputs = { self }: {
    # For Linux
    legacyPackages.x86_64-linux = import ./default.nix {
      system = "x86_64-linux";
    };

    # For macOS (Apple Silicon - aarch64)
    legacyPackages.aarch64-darwin = import ./default.nix {
      system = "aarch64-darwin";
    };
  };
}
