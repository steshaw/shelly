#
# HT https://gist.github.com/LnL7/570349866bb69467d0caf5cb175faa74
#
# Initially install with
#  $ nix-env -f '<nixpkgs>' -r -iA userPackages
#
# Afterwards, edit the file and run:
#
#  $ nix-rebuild
#
# See https://gist.github.com/LnL7/570349866bb69467d0caf5cb175faa74#gistcomment-2598159
#
# See also:
#  https://gist.github.com/Widdershin/de023630617c405b033245ced16221f2
#
# Found at
#   https://nixos.wiki/wiki/FAQ#Why_not_use_nix-env_-i_foo.3F
#
self: super:
with builtins; rec {

  userPackages = super.userPackages or {} // super.recurseIntoAttrs {
#  userPackages = super.userPackages or {} // {

#    Agda = super.haskellPackages.Agda; # Agda is broken.

    #
    # Kind of like build-essential.
    #
    # For example needed to build some NPM packages.
    #
    # FIXME: Cannot `yarn global add gitmoji` because kexec doesn't build.
    #
    inherit (self)
      gcc
      gnumake
      python
    ;

    #
    # Hmmm
    #
    yarn = (super.yarn.override {
      nodejs = super.nodejs-10_x;
    });
    inherit (self)
      nodejs-10_x
    ;

    #
    # My favourite command line apps.
    #
    inherit (self)
      bashInteractive_5
      bat
      bind # for dig
      cabal2nix
      direnv
      emacs
      fd
      fzf
      gist
      git
      gnupg22
      hledger
      htop
      httpie
      jq
      mr
      neofetch
#      neovim # Deferring to Vim for now.
      ripgrep
      rlwrap
      shellcheck
      stack
      tmux
      tree
      watchman
      youtube-dl
      zsh
    ;

    # XXX: Do I need to call this `my_vim` instead?
    vim = (super.vim_configurable.override {
      guiSupport = "no";
      darwinSupport = super.stdenv.isDarwin;
      python = super.python3;
    }).overrideAttrs (prevAttrs: {
      name = "my-vim-${prevAttrs.version}";
    });

    tmux-fzf-url = self.tmuxPlugins.fzf-tmux-url;

    #
    # GUI apps and fonts.
    #
    inherit (self)
      fira-code
      source-code-pro
    ;

    #
    # Default packages.
    #
    # WARNING: Do not remove!
    #
    inherit (self)
      nix # Don't enable this on multi-user.
      cacert;

    #
    # nix-rebuild script.
    #
    nix-rebuild = super.writeScriptBin "nix-rebuild" ''
      #!${super.stdenv.shell}
      if ! command -v nix-env &>/dev/null; then
          echo "warning: nix-env was not found in PATH, add nix to userPackages" >&2
          PATH=${self.nix}/bin:$PATH
      fi
      exec nix-env -f '<nixpkgs>' -r -iA userPackages "$@"
    '';

    #
    # FIXME: Fix below which provides a diff but breaks with use of `super.recurseIntoAttrs` above.
    #        The `super.recurseIntoAttrs` allows you to find userPackages when doing `nix search`.
/*
    nix-rebuild = super.writeScriptBin "nix-rebuild" ''
      #!${super.stdenv.shell}

#      #!${super.stdenv.shell}
#      if ! command -v nix-env &>/dev/null; then
#        echo "warning: nix-env was not found in PATH, add nix to userPackages" >&2
#        PATH=${self.nix}/bin:$PATH
#      fi
#      exec nix-env -f '<nixpkgs>' -r -iA userPackages "$@"

      #!${super.stdenv.shell}
      set -e
      if ! command -v nix-env &>/dev/null; then
        echo "warning: nix-env was not found in PATH, add nix to userPackages" >&2
        PATH=${self.nix}/bin:$PATH
      fi
      IFS=- read -r _ oldGen _ <<<"$(readlink "$(readlink ~/.nix-profile)")"
      oldVersions=$(readlink ~/.nix-profile/package_versions || echo "/dev/null")
      nix-env -f '<nixpkgs>' -r -iA userPackages "$@"
      IFS=- read -r _ newGen _ <<<"$(readlink "$(readlink ~/.nix-profile)")"
      ${self.diffutils}/bin/diff --color -u --label "generation $oldGen" $oldVersions \
        --label "generation $newGen" ~/.nix-profile/package_versions \
        || true
    '';

    packageVersions =
      let
        versions = super.lib.attrsets.mapAttrsToList (_: pkg: pkg.name) self.userPackages;
        versionText = super.lib.strings.concatMapStrings (s: s+"\n") versions;
      in
      super.writeTextDir "package_versions" versionText;
*/
  };
}
