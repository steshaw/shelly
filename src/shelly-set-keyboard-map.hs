import qualified Data.List as L
import System.Environment (getArgs)
import System.Exit (exitSuccess)

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

-- Show usage information
showUsage :: IO ()
showUsage = do
  putStrLn "Usage: shelly-set-keyboard-map [OPTION]"
  putStrLn ""
  putStrLn "Generate a localectl command to set X11 keymap with custom options."
  putStrLn ""
  putStrLn "Options:"
  putStrLn "  --apple        Use Apple keyboard settings (default)"
  putStrLn "  --pc           Use PC keyboard settings"
  putStrLn "  --non-apple    Use PC keyboard settings (same as --pc)"
  putStrLn "  --help         Show this help message"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  shelly-set-keyboard-map           # Use Apple settings (default)"
  putStrLn "  shelly-set-keyboard-map --apple   # Explicitly use Apple settings"
  putStrLn "  shelly-set-keyboard-map --pc      # Use PC settings"
  exitSuccess

-- Parse command line arguments to determine if we're on Apple hardware
parseIsApple :: [String] -> IO Bool
parseIsApple [] = return True  -- Default to Apple if no arguments provided
parseIsApple ("--help":_) = showUsage >> return True  -- This won't be reached due to exitSuccess
parseIsApple ("--apple":_) = return True
parseIsApple ("--pc":_) = return False
parseIsApple ("--non-apple":_) = return False
parseIsApple (_:rest) = parseIsApple rest

generateCommand :: Bool -> String
generateCommand isApple =
  let otherOptions = if isApple then appleOptions else nonAppleOptions
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
  in "localectl --no-convert set-x11-keymap us pc105 \"\" "
       ++ L.intercalate "," options

main :: IO ()
main = do
  args <- getArgs
  isApple <- parseIsApple args
  putStrLn $ generateCommand isApple
