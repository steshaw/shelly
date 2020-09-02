{ config, pkgs, lib, ... }:
{
  # FIXME: Not Xorg.
  programs.sway.enable = false;

/*
  environment = {
    etc."xdg/gtk-2.0/gtkrc" = {
      text = ''
        gtk-icon-theme-name = "Adwaita"
        gtk-theme-name = "Arc-Dark"
        gtk-cursor-theme-name = "Adwaita"
        gtk-fallback-icon-theme = "gnome"
        gtk-font-name = "DejaVu Sans 11"
      '';
      mode = "444";
    };
    etc."xdg/gtk-3.0/settings.ini" = {
      text = ''
        [Settings]
        gtk-icon-theme-name=Arc
        gtk-theme-name=Arc-Dark
        gtk-cursor-theme-name=Adwaita
        gtk-fallback-icon-theme=gnome
        gtk-font-name = Noto Sans 11
      '';
      mode = "444";
    };
  };
*/

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

    # configure lightdm
    displayManager = {
      lightdm = {
        enable = true;
        greeters = {
          gtk = {
            #indicators = [ "~clock" "~session" "~power" ];
            theme = {
              name = "Adwaita-Dark";
            };
          };
        };
      };
    };

    displayManager = {
      defaultSession = "none+i3";
      #default = "gnome-flashback-gnome3-i3-xsession";
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
        konsole
      ];
    };
  };

  # Enable Qt5 integration.
  qt5 = {
    enable = true;
    platformTheme = "gtk2";
    style = "gtk2";
  };
  environment.extraInit = ''
    export QT_STYLE_OVERRIDE="Adwaita-Dark"
  '';

  services.gnome3.gnome-keyring.enable = true;

  environment.mate.excludePackages = with pkgs.mate; [
    mate-calc
    mate-terminal
    pluma
  ];

  security.pam.services.sddm.enableKwallet = true;
  security.pam.services.kdewallet.enableKwallet = true;
  environment.systemPackages = with pkgs; [

  qgnomeplatform adwaita-qt
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
