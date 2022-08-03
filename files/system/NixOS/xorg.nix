{ config, pkgs, lib, ... }:
let
  myLocker = "${pkgs.i3lock}/bin/i3lock -c000000 -i ~/.background.png";
  uPkgs = import
    (builtins.fetchGit {
      url = "https://github.com/NixOS/nixpkgs";
      #rev = "3c0e3697520cbe7d9eb3a64bfd87de840bf4aa77";
      rev = "dfd2eeabd6e1be22676dac26854a2de21c3d4c87";
    })
    {
      config = {
        allowUnfree = true;
      };
    };
in
{
  programs.xss-lock = {
    enable = true;
  };

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
      sessionCommands =
        let
          shellyXInput = pkgs.writeScript "shelly-xinput"
            (builtins.readFile ./script/shelly-xinput);
        in
        ''
          ${pkgs.xss-lock}/bin/xss-lock -- ${myLocker} &
          ${pkgs.dropbox}/bin/dropbox &
          ${pkgs.autokey}/bin/autokey-gtk &
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
        i3lock
        i3status
        networkmanager_dmenu
      ];
    };

    xautolock = {
      enable = true;
      time = 5;
      locker = "${myLocker}";
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
    # For themeing Gnome/Gtk/Qt apps.
    qgnomeplatform
    adwaita-qt

    # Apps.
    brave
    dropbox
    firefox
    google-chrome
    uPkgs.vscode
    zotero

    # Terminal emulators.
    alacritty
    kitty
    konsole

    # KDE apps.
    gwenview # Image viewer.
    kdeFrameworks.kwallet
    ksshaskpass
    okular # PDF viewer.
    spectacle # Screenshots.

    # Remote desktops. None work well.
    #    nomachine-client # no nomachine-server :-(.
    #    teamviewer
    #    tigervnc
    #    x11vnc

    # X tools.
    autokey
    libinput
    libnotify
    networkmanager_dmenu
    xdotool
    xorg.mkfontdir
    xorg.mkfontscale
    xorg.xdpyinfo
    xorg.xev
    xorg.xinput
    xorg.xmodmap
    xorg.xrandr
    xsel
  ];
}
