{ config, pkgs, lib, ... }:
{
  # FIXME: Not Xorg.
  programs.sway.enable = false;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us";
    # Enable touchpad support.
    libinput = {
      enable = true;
      naturalScrolling = true;
      accelProfile = "adaptive";
      disableWhileTyping = true;
    };

    displayManager.sddm.enable = false;

    displayManager = {
      lightdm = {
        enable = true;
        greeters.enso.enable = false;
      };
    };

    displayManager = {
      defaultSession = "none+i3";
    };

    # Enable the KDE Desktop Environment?
    desktopManager.plasma5.enable = false;

    # Enable the Gnome 3 Desktop Environment?
    desktopManager.gnome3.enable = false;

    # Enable MATE?
    desktopManager.mate.enable = false;

    # Xmonad
    windowManager.xmonad.enable = false;
    windowManager.xmonad.enableContribAndExtras = false;

    windowManager.i3 = {
      enable = true;
      extraPackages = with pkgs; [
        dmenu
        i3status
        i3lock
        #i3blocks
      ];
    };
  };

  services.gnome3.gnome-keyring.enable = true;

  environment.mate.excludePackages = with pkgs.mate; [
    mate-calc
    mate-terminal
    pluma
  ];

  security.pam.services.sddm.enableKwallet = true;
  security.pam.services.kdewallet.enableKwallet = true;
  environment.systemPackages = with pkgs; [
    # Apps.
    brave
    dropbox
    firefox
    google-chrome
    libinput
    rescuetime
    vscode

    # Terminal emulators.
    alacritty
    kitty
    konsole

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
    xdotool
    xorg.mkfontdir
    xorg.mkfontscale
    xorg.xev
    xorg.xmodmap
    xorg.xrandr
    xsel

    # Xmonad apps.
    dmenu
    gmrun
    lxqt.pavucontrol-qt # Audio controls.
    xscreensaver
  ];
}
