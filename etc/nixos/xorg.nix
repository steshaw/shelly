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
      accelProfile = "adaptive";
      disableWhileTyping = true;
    };
/*
    inputClassSections = [
      ''
        Identifier "Mouse"
          MatchProduct "Dell Dell Universal Receiver Mouse"
          Option "Accel Profile Enabled" "1 0"
          Option 'Accel Speed' "-1.0"
          Option "NaturalScrolling" "true"
      ''
    ];
*/

    displayManager = {
      sddm.enable = true;

      setupCommands = ''
        exec >/tmp/xserver-setup-commands.log 2>&1
        PATH=/home/steshaw/Code/steshaw/shelly/scripts:/run/current-system/sw/bin:$PATH
        su -c 'xserver-setup-commands' steshaw
      '';
    };

    # Enable the KDE Desktop Environment.
    desktopManager.plasma5.enable = false;

    # Enable the Gnome 3 Desktop Environment.
    desktopManager.gnome3.enable = false;

    # Enable MATE.
    desktopManager.mate.enable = true;

    # Xmonad
    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;
  };

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
    libinput
    lxqt.pavucontrol-qt
    rescuetime
    slack

    #
    # Terminals.
    #
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
    xorg.xev
    xorg.xmodmap
    xorg.xrandr
    xsel

    # Xmonad apps.
    dmenu
    gmrun
    xscreensaver
  ];
}
