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

    displayManager.sddm.enable = true;

    # Enable the KDE Desktop Environment.
    desktopManager.plasma5.enable = true;

    # Enable the Gnome 3 Desktop Environment.
    desktopManager.gnome3.enable = true;

    # Enable MATE.
    desktopManager.mate.enable = true;

    # Xmonad
    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;
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

    # Xmonad
    dmenu
    gmrun
  ];
}
