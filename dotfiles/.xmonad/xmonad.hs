module Main where

import XMonad
import XMonad.Config.Kde
import XMonad.Config.Mate
import XMonad.Layout.Cross (simpleCross)
import XMonad.Layout.StackTile (StackTile (..) )
import XMonad.Layout.Tabbed (simpleTabbed)
import XMonad.Layout.ThreeColumns (ThreeCol (..) )
import XMonad.Util.EZConfig (additionalKeys)
import qualified XMonad.StackSet as W -- to shift and float windows

import Data.Semigroup (Endo)

myTerminal :: String
myTerminal = let useAlacritty = False in
  if useAlacritty
    then "alacritty"
    else "konsole"

myNormalBorderColor  = "#999999"
myFocusedBorderColor = "#0066cc"

myWorkspaces :: [String]
myWorkspaces = ws
  where
    named = ["mail"]
    nums = map show [1..9::Int]
    ws = named ++ drop (length named) nums

myManageHook :: Query (Endo WindowSet)
myManageHook =
  composeAll . concat $
    [ [className =? c --> doFloat | c <- myFloatClassNames],
      [title =? t --> doFloat | t <- myFloatTitles],
      [className =? c --> doF (W.shift "2") | c <- webApps],
      [className =? c --> doF (W.shift "3") | c <- ircApps]
    ]
  where
    myFloatClassNames = ["mate-screenshot", "spectacle"]
    myFloatTitles = ["alsamixer"]
    webApps = ["Firefox-bin", "Opera"] -- open on desktop 2
    ircApps = ["Ksirc"] -- open on desktop 3

stackTile = StackTile 1 (3/100) (1/2)

threeCol = ThreeCol 1 (3/100) (1/2)

myBasicConfig = def
  { modMask = myModMask
  , terminal = myTerminal
  , workspaces = myWorkspaces
  , borderWidth = 2
  , normalBorderColor  = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
  , layoutHook = layoutHook def ||| simpleTabbed ||| threeCol ||| stackTile
  , manageHook = manageHook def <+> myManageHook
  }
  `additionalKeys` myKeys
  where
    myKeys = [ ((myModMask, xK_Print), spawn "mate-screenshot")]
    myModMask = mod4Mask

-- |
--
-- Helpful KDE 5 references:
--
-- * <https://wiki.haskell.org/Xmonad/Using_xmonad_in_KDE>
-- * <https://github.com/marbu/xmonad/issues/1>
--
myKdeConfig = kdeConfig
              { modMask = mod4Mask, -- use the Windows button as mod
                manageHook = manageHook kdeConfig <+> myManageHook
              }

myMateConfig = mateConfig
  { modMask = myModMask
  , terminal = myTerminal
  , layoutHook = layoutHook mateConfig ||| simpleTabbed ||| threeCol ||| stackTile

  -- TODO: Share this configuration.
  , borderWidth = 2
  , normalBorderColor  = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
  , workspaces = myWorkspaces
  }
  `additionalKeys` myKeys
  where
    myKeys = [ ((myModMask, xK_Print), spawn "mate-screenshot")]
    myModMask = mod4Mask

data MyConfig = Basic | KDE | MATE

-- TODO: Type error!
{-
myConfig Basic = myBasicConfig
myConfig KDE = myKdeConfig
myConfig MATE = myMateConfig
-}

main :: IO ()
main = xmonad myBasicConfig
