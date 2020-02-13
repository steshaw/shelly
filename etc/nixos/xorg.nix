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
    # Apps.
    brave
    firefox
    libinput
    lxqt.pavucontrol-qt
    dropbox
    rescuetime

    # KDE apps.
    gwenview # Image viewer.
    kdeFrameworks.kwallet
    ksshaskpass
    okular
    spectacle # Screenshot taker.

    # Remote desktops. None work well.
#    nomachine-client # no nomachine-server :-(.
#    teamviewer
#    tigervnc
#    x11vnc

    # X Tools
    xsel
    xdotool
    xorg.xev
    xorg.xmodmap
    xorg.mkfontdir
    xorg.mkfontscale
    xorg.xev
    xorg.xrandr

    # Xmonad apps.
    dmenu
    gmrun
  ];
}
