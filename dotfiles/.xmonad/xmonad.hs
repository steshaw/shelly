module Main where

import XMonad
import XMonad.Config.Kde
import XMonad.Config.Mate
import qualified XMonad.StackSet as W -- to shift and float windows

import Data.Semigroup (Endo)

myTerminal :: String
myTerminal = let useAlacritty = True in
  if useAlacritty
    then "alacritty"
    else "konsole"

myBasicConfig = def {modMask = mod4Mask, terminal = myTerminal}

-- |
--
-- Helpful KDE 5 references:
--
-- * <https://wiki.haskell.org/Xmonad/Using_xmonad_in_KDE>
-- * <https://github.com/marbu/xmonad/issues/1>
myKdeConfig = kdeConfig
              { modMask = mod4Mask, -- use the Windows button as mod
                manageHook = manageHook kdeConfig <+> myManageHook
              }

myMateConfig = mateConfig {modMask = mod4Mask, terminal = myTerminal, borderWidth = 50}

myManageHook :: Query (Endo WindowSet)
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

data MyConfig = Basic | KDE | MATE

-- TODO: Type error!
{-
myConfig Basic = myBasicConfig
myConfig KDE = myKdeConfig
myConfig MATE = myMateConfig
-}

main :: IO ()
main = xmonad myMateConfig
