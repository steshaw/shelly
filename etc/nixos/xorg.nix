{ config, pkgs, lib, ... }:
{
  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us";
    # Enable touchpad support.
    libinput = {
      enable = true;
      naturalScrolling = true;
    };

    # Enable the KDE Desktop Environment.
#    displayManager.lightdm.enable = true; # default displayManager.
    displayManager.sddm.enable = true;
    desktopManager.plasma5.enable = true;

  };
  security.pam.services.sddm.enableKwallet = true;
  security.pam.services.kdewallet.enableKwallet = true;
  environment.systemPackages = with pkgs; [
    brave
    firefox
    libinput
    lxqt.pavucontrol-qt
    xorg.xev
    xorg.xmodmap

    # KDE
    kdeFrameworks.kwallet
    ksshaskpass
    okular
  ];
}
