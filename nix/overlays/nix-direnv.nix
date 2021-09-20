self: super: {
  nix-direnv = super.nix-direnv.override {
    enableFlakes = true;
  };
}
