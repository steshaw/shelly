import qualified Data.List as L

isApple = True

appleOptions =
  [ -- Swap Ctrl and Win (command and control).
    "ctrl:swap_lwin_lctl,ctrl:swap_rwin_rctl"
  , -- Apple Aluminium: emulate PC keys (PrtSc, Scroll Lock, Pause, Num Lock)
    "apple:alupckeys"
  , -- Numeric keypad always enters digits (as in macOS).
    "numpad:mac"
  , -- Ctrl is mapped to Win and the usual Ctrl keys.
    "altwin:ctrl_win"
  ]

nonAppleOptions = ["ctrl:swap_lalt_lctl_lwin"]

otherOptions = if isApple then appleOptions else nonAppleOptions

options =
  [ -- Make unmodified Caps Lock an additional Esc,
    -- but Shift + Caps Lock behaves like regular Caps Lock.
    "caps:escape_shifted_capslock"
  , -- Both Shift together enable Caps Lock.
    "shift:both_capslock_cancel"
  , -- Compose is Right Alt or Scroll Lock.
    "compose:ralt,compose:sclk"
  , -- Use Enter to choose the 3rd level.
    "lv3:enter_switch"
  ]
    ++ otherOptions

cmd =
  "localectl --no-convert set-x11-keymap us pc105 \"\" "
    ++ L.intercalate "," options

main = putStrLn cmd
