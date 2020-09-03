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
    xkbVariant = "";
    xkbOptions = lib.concatStringsSep "," [
      # Make unmodified Caps Lock an additional Esc, but Shift + Caps Lock
      # behaves like regular Caps Lock.
      "caps:escape_shifted_capslock"

      # Both Shift together enable Caps Lock.
      "shift:both_capslock"

      # Swap Ctrl and Win (command and control).
      "ctrl:swap_lwin_lctl,ctrl:swap_rwin_rctl"

      # Compose is Right Alt or Scroll Lock.
      "compose:ralt,compose:sclk"

      # Apple Aluminium: emulate PC keys (PrtSc, Scroll Lock, Pause, Num Lock)
      "apple:alupckeys"

      # Numeric keypad always enters digits (as in macOS).
      "numpad:mac"

      # Use Enter to choose the 3rd level.
      "lv3:enter_switch"

      # Ctrl is mapped to Win and the usual Ctrl keys.
      "altwin:ctrl_win"

      #
      # Swap Alt and Win
      # "altwin:swap_alt_win"
      # "altwin:swap_alt_win"
      #
    ];

    # Enable touchpad support.
    libinput = {
      enable = true;
      naturalScrolling = true;
      accelProfile = "adaptive";
      disableWhileTyping = true;
    };

    displayManager = {
      defaultSession = "none+i3";
      #default = "gnome-flashback-gnome3-i3-xsession";
      sddm.enable = false;
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
      sessionCommands = let
        shellyXInput = pkgs.writeScript "shelly-xinput"
          (builtins.readFile ./scripts/shelly-xinput);
      in
        ''
          ${pkgs.rescuetime}/bin/rescuetime &
          ${pkgs.dropbox}/bin/dropbox &
          echo before xinput
          ${shellyXInput}
          echo after xinput
        '';
    };

    # Enable the KDE Desktop Environment?
    desktopManager.plasma5.enable = false;

    # Enable the Gnome 3 Desktop Environment?
    desktopManager.gnome3.enable = false;

    # Enable MATE?
    desktopManager.mate.enable = false;

    # Enable Xmonad?
    windowManager.xmonad = {
      enable = false;
      enableContribAndExtras = false;
      extraPackages = with pkgs; [
        dmenu
        lxqt.pavucontrol-qt # Audio controls.
      ];
    };

    # Enable i3.
    windowManager.i3 = {
      enable = true;
      extraPackages = with pkgs; [
        dmenu
        networkmanager_dmenu
        i3status
        #i3lock
        #i3blocks
        i3lock
        xss-lock
      ];
    };

    /*
    xautolock = {
      enable = true;
      time = 1;
      locker = "${pkgs.i3lock}/bin/i3lock -c000000 -i ~/Pictures/background.png";
    };
    */
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
    # For themeing Gnome/Gtk/Qt apps.
    qgnomeplatform
    adwaita-qt

    # Apps.
    brave
    firefox
    google-chrome
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
    spectacle # Screenshots.

    # Remote desktops. None work well.
    #    nomachine-client # no nomachine-server :-(.
    #    teamviewer
    #    tigervnc
    #    x11vnc

    # X tools.
    libinput
    libnotify
    networkmanager_dmenu
    xdotool
    xorg.mkfontdir
    xorg.mkfontscale
    xorg.xev
    xorg.xinput
    xorg.xmodmap
    xorg.xrandr
    xsel
    xss-lock
  ];
}
