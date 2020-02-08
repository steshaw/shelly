import XMonad

main = xmonad defaultConfig {-modMask = mod4Mask,-} {terminal = "alacritty"}

--
-- Some KDE config that could be useful.
--
-- From:
-- https://wiki.haskell.org/Xmonad/Using_xmonad_in_KDE
--
{-

import XMonad
import XMonad.Config.Kde
import qualified XMonad.StackSet as W -- to shift and float windows

main = xmonad kdeConfig
    { modMask = mod4Mask -- use the Windows button as mod
    , manageHook = manageHook kdeConfig <+> myManageHook
    }

myManageHook = composeAll . concat $
    [ [ className   =? c --> doFloat           | c <- myFloats]
    , [ title       =? t --> doFloat           | t <- myOtherFloats]
    , [ className   =? c --> doF (W.shift "2") | c <- webApps]
    , [ className   =? c --> doF (W.shift "3") | c <- ircApps]
    ]
  where myFloats      = ["MPlayer", "Gimp"]
        myOtherFloats = ["alsamixer"]
        webApps       = ["Firefox-bin", "Opera"] -- open on desktop 2
        ircApps       = ["Ksirc"]                -- open on desktop 3

-}
