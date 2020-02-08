module Main where

import XMonad
import XMonad.Config.Kde
import qualified XMonad.StackSet as W -- to shift and float windows

-- |
--
-- Helpful KDE 5 references:
--
-- * <https://wiki.haskell.org/Xmonad/Using_xmonad_in_KDE>
-- * <https://github.com/marbu/xmonad/issues/1>
main =
  let enableKde = False
   in if enableKde
        then
          xmonad
            kdeConfig
              { modMask = mod4Mask, -- use the Windows button as mod
                manageHook = manageHook kdeConfig <+> myManageHook
              }
        else-- Really nice basic configuration.
          let
            alacrittyBroken = True
            terminal = if alacrittyBroken then "kconsole" else "alacritty"
          in
            xmonad defaultConfig {modMask = mod4Mask, terminal = terminal}

myManageHook =
  composeAll . concat $
    [ [className =? c --> doFloat | c <- myFloats],
      [title =? t --> doFloat | t <- myOtherFloats],
      [className =? c --> doF (W.shift "2") | c <- webApps],
      [className =? c --> doF (W.shift "3") | c <- ircApps]
    ]
  where
    myFloats = ["MPlayer", "Gimp"]
    myOtherFloats = ["alsamixer"]
    webApps = ["Firefox-bin", "Opera"] -- open on desktop 2
    ircApps = ["Ksirc"] -- open on desktop 3
