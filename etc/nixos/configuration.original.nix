# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nixpkgs.config.allowUnfree = true;

  # Use the systemd-boot EFI boot loader.
  boot.supportedFilesystems = [ "zfs" ];
  services.zfs.autoScrub.enable = true;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "verona";
  networking.hostId = "df28ea3c";
#  networking.wireless.enable = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  # Set your time zone.
  time.timeZone = "Australia/Brisbane";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
     firefox
     git
     vim
     wget
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us";
    # xkbOptions = "eurosign:e";
    # Enable touchpad support.
    libinput.enable = true;

#    windowManager.xmonad {
#      enable = true;
#      enableContribAndExtras = true;
#    };
    # Enable the KDE Desktop Environment.
    displayManager.sddm.enable = true;
    desktopManager.plasma5.enable = true;

    # Enable the Gnome desktop environment.
#    displayManager.gdm.enable = true;
#    displayManager.gdm.wayland = false;
#    desktopManager.gnome3.enable = true;

#    displayManager.lightdm.enable = true;
  };


  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.steshaw = {
    isNormalUser = true;
    home = "/home/steshaw";
    description = "Steven Shaw";
    extraGroups = [ "wheel" "networkmanager" ];
    openssh.authorizedKeys.keys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDQM+CdduNpScMe8Uucb5TCVLx3HrXUrTJO8hBkOF8Dy4+IYLFxo6teT8XT4X0+SLJ6gdxPjRPfqmcZwdg051BkaPAh6TkX0zqAeaBFSh3rVmNWV1mxDxYl9X5yWXkaj/kCOpJccz2NrNINYvv4wYHVoVRDg97+RMCdLyzXV2W0sf+J1Mozpj05AAgo6iqUNwo8bHJtekD4UZ6L1Zql3QSwBZvIo2eK9Ir6DPhDMRD0YHLUnFfdLYbGhlqi3qPu8CbEbu+4ptyhlePqvIymdmVwd2VL44SBr5KvlmFuZmhm/LL2b89tb2a2X3RW8do4y1W/wIOwfSeUfXf83zUftTyR steven@steshaw.org" ];
  };

  security.sudo.wheelNeedsPassword = false;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?
}