{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
#    <nixos/modules/virtualisation/virtualbox-image.nix>
    <nixos/modules/installer/cd-dvd/channel.nix>
    <nixos/modules/profiles/clone-config.nix>
#    <nixos/modules/profiles/graphical.nix>
  ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";

  networking.hostName = "nice";
  networking.hostId = "ff6ad811";

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "lat9w-16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

#  users.mutableUsers = false;
  users.extraUsers.steshaw = {
    isNormalUser = true;
    description = "Steven Shaw";
    group = "users";
    extraGroups = [ "wheel" "vboxsf" ];
    password = "steshaw";
    uid = 1000;
    shell = "${pkgs.zsh}/bin/zsh";
  };

  services.xserver = {
    enable = true;
#    startGnuPgAgent = true;
    desktopManager = {
#      auto.enable = true;
#      auto.user = "steshaw";
      kde5.enable = true;
    };
  };

#  fileSystems."/media/steshaw" = {
#    fsType = "vboxsf";
#    device = "vboxsf";
#    options = "uid=1000,gid=73,rw";
#  };

  nix.trustedBinaryCaches = [ 
#    http://hydra.nixos.org
#    http://cache.nixos.org
    http://hydra.cryp.to
  ];
  nix.binaryCaches = [ 
#    http://hydra.nixos.org
#    http://cache.nixos.org
    http://hydra.cryp.to
  ];

  virtualisation.docker.enable = true;
  security.sudo.wheelNeedsPassword = false;

  environment.systemPackages = with pkgs; [
    vim
    git
    which
    python
    gnupg
  ];
}
