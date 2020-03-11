# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ ./hardware-configuration.nix

      ./cachix.nix
      ./users.nix
      ./xorg.nix
      ./dnsmasq.nix
    ];

  nix.useSandbox = true;
  nixpkgs.config.allowUnfree = true;
  virtualisation.docker.enable = true;
  systemd.extraConfig = "DefaultLimitNOFILE=1048576";

  # Use the systemd-boot EFI boot loader.
  boot.supportedFilesystems = [ "zfs" ];
  services.zfs.autoScrub.enable = true;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # TODO: Perhaps swapfile's are not compatible with ZFS.
#  swapDevices = [ { device = "/var/swapfile"; size = 32768; } ];
#  boot.resumeDevice = "rpool/root/nixos";

  time.timeZone = "Australia/Brisbane";

  # ------------------------------------------------------------------------
  # Services
  # ------------------------------------------------------------------------
  services.avahi.enable = true;
  services.avahi.nssmdns = true;
  services.avahi.publish.enable = true;
  services.avahi.publish.addresses = true;

  services.emacs.enable = true;
  services.eternal-terminal.enable = true;
  services.keybase.enable = true;

  # ------------------------------------------------------------------------
  # Network proxy
  # ------------------------------------------------------------------------
  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # ------------------------------------------------------------------------
  # Internationalisation
  # ------------------------------------------------------------------------
  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  # ------------------------------------------------------------------------
  # System packages
  # ------------------------------------------------------------------------
  environment.systemPackages = with pkgs; [
    direnv
    git
    poppler # For Emacs pdf-tools — Spacemacs pdf layer.
    vim
    wget

    #
    # Haskell packages.
    #

    # Install stable HIE for GHC 8.6.5.
    #
    # https://github.com/infinisil/all-hies/
    #
    (
      let
        enable_hie = true;
        u = "https://github.com/infinisil/all-hies/tarball/master";
        all-hies = import (fetchTarball u) {};
      in
      if enable_hie then all-hies.selection {
        selector = p: {
          inherit (p)
            ghc882
            ghc865
          ;
        };
      } else cachix
    )
    haskellPackages.brittany
    cachix
    haskell.compiler.ghc865
    hlint
  ];

  # ------------------------------------------------------------------------
  # Buildkite
  # ------------------------------------------------------------------------
/*
  services.buildkite-agent.enable = false;
  services.buildkite-agent.openssh.privateKeyPath = /tmp/buildkite-agent/buildkite_rsa;
  services.buildkite-agent.openssh.publicKeyPath = /tmp/buildkite-agent/buildkite_rsa.pub;
  services.buildkite-agent.tokenPath = /tmp/buildkite-agent/token;
*/

  # ------------------------------------------------------------------------
  # mtr
  # ------------------------------------------------------------------------
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.mtr.enable = false;

  # ------------------------------------------------------------------------
  # GNU Privacy Guard
  # ------------------------------------------------------------------------
  #programs.gnupg.agent.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # ------------------------------------------------------------------------
  # OpenSSH daemon
  # ------------------------------------------------------------------------
  services.openssh = {
    enable = true;
    forwardX11 = true;
    permitRootLogin = "no";
    passwordAuthentication = false;
  };

  # ------------------------------------------------------------------------
  # Networking
  # ------------------------------------------------------------------------
  networking.hostName = "verona";
  networking.hostId = "df28ea3c";
  networking.wireless.enable = true;
/*
  networking.networkmanager = {
    enable = true;
    dns = "dnsmasq";
    unmanaged = ["boo"];
  };
*/

  #
  # Open ports in the firewall.
  #
  # No need to specify 22 here when the openssh service is enabled.
  # https://nixos.org/nixos/manual/index.html#sec-firewall
  #
  networking.firewall.enable = false;
  networking.firewall.allowPing = true;
  networking.firewall.allowedTCPPorts = [
    80 433 # HTTP ports
    5900 # VNC
    3389 # RDP
    6568 7070 # AnyDesk
  ];
  networking.firewall.allowedTCPPortRanges = [
    { from = 3000; to = 3010; } # Dev ports.
    { from = 8000; to = 8081; } # Dev ports.
  ];

  # ------------------------------------------------------------------------
  # Printing
  #
  # CUPS for printing documents.
  #
  # ------------------------------------------------------------------------
  services.printing.enable = false;

  # ------------------------------------------------------------------------
  # Sound and Bluetooth
  # ------------------------------------------------------------------------
  sound.enable = true;
  hardware.bluetooth.enable = true;
  hardware.pulseaudio = {
    enable = true;

    # NixOS allows either a lightweight build (default) or full build of
    # PulseAudio to be installed.  Only the full build has Bluetooth support, so
    # it must be selected here.
    package = pkgs.pulseaudioFull;
  };
  services.blueman.enable = true;

  # ------------------------------------------------------------------------
  # Fonts
  # ------------------------------------------------------------------------
  fonts.fonts = with pkgs; [
    noto-fonts-emoji
    font-awesome_5
    source-code-pro
    fira-code
  ];

  # ------------------------------------------------------------------------
  # sudo
  # ------------------------------------------------------------------------
  security.sudo.wheelNeedsPassword = false;
  nix.trustedUsers = ["@wheel" "steshaw"];

  # ------------------------------------------------------------------------
  # DANGER
  # ------------------------------------------------------------------------
  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # NOTE: Did you read the comment?
}
