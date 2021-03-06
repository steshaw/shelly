# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
let enableTailscale = false;
in
{
  imports =
    [
      ./hardware-configuration.nix

      ./cachix.nix
      ./users.nix
      ./xorg.nix
      ./dnsmasq.nix
    ];

  nix.useSandbox = true;

  # Nix options for derivations to persist garbage collection.
  # See https://github.com/nix-community/nix-direnv
  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
  '';

  nixpkgs.config.allowUnfree = true;

  virtualisation.docker.enable = true;
  virtualisation.virtualbox.host = {
    enable = true;
    enableExtensionPack = true;
  };
  systemd.extraConfig = "DefaultLimitNOFILE=1048576";

  # Use the systemd-boot EFI boot loader.
  boot.supportedFilesystems = [ "zfs" ];
  services.zfs.autoScrub.enable = true;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # FIXME: Perhaps swap files are not compatible with ZFS?
  #  swapDevices = [{
  #    device = "/var/swap";
  #    size = 32768; # MiB
  #  }];
  #boot.resumeDevice = "rpool/root/nixos";

  time.timeZone = "Australia/Brisbane";

  documentation.man = {
    enable = true;
    # TODO: Only in 20.09.
    # generateCaches = true;
  };

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

  services.tailscale.enable = enableTailscale;
  services.lorri.enable = true;

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
    neovim
    nix-index
    perl
    poppler # For Emacs pdf-tools — Spacemacs pdf layer.
    vim
    wget

    (if enableTailscale then tailscale else git)

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
        #r = "4b6aab017cdf96a90641dc287437685675d598da"; # Mar 13
        #r = "92148680060ed68f24738128d8489f4f9387d2ff"; # Feb 2
        #r = "67227c42ab0b652775dbaf6401f8eff71f5d0c4f"; # Jan 30
        # haskell.nix branch, Jun 8, 2020.
        r = "815746252771eb7e745b03920a239324398ce79f";
        u = "https://github.com/infinisil/all-hies/tarball/${r}";
        all-hies = import (fetchTarball u) { };
      in
      if enable_hie then
        all-hies.selection
          {
            selector = p: {
              inherit (p)
                #            ghc882
                ghc865
                ;
            };
          } else { }
    )
    cachix
    haskell.compiler.ghc865
    haskellPackages.brittany
    hlint
    (if false then (import (builtins.fetchTarball "https://github.com/cachix/ghcide-nix/tarball/master") { }).ghcide-ghc865 else cachix)
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
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryFlavor = "tty";
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
    80
    433 # HTTP ports
    5900 # VNC
    3389 # RDP
    6568
    7070 # AnyDesk
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
    # PulseAudio to be installed. Only the full build has Bluetooth support,
    # so it must be selected here.
    package = pkgs.pulseaudioFull;
  };
  services.blueman.enable = true;

  # ------------------------------------------------------------------------
  # Fonts
  # ------------------------------------------------------------------------
  fonts.fonts = with pkgs; [
    corefonts
    fira-code
    font-awesome_5
    nerdfonts
    noto-fonts-emoji
    source-code-pro
    jetbrains-mono
  ];

  # ------------------------------------------------------------------------
  # sudo
  # ------------------------------------------------------------------------
  security.sudo.wheelNeedsPassword = false;
  nix.trustedUsers = [ "@wheel" "steshaw" ];

  # ------------------------------------------------------------------------
  # DANGER
  # ------------------------------------------------------------------------
  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # NOTE: Did you read the comment?
}
