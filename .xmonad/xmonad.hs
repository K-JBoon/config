{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

import qualified Codec.Binary.UTF8.String as UTF8
import Data.Monoid

import XMonad
import qualified Data.Map as M
import Data.List
import Graphics.X11.ExtraTypes.XF86
import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageHelpers
import qualified XMonad.Layout.Fullscreen as FS
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.DynamicLog
import System.IO
import Control.Monad
import qualified XMonad.StackSet as W

import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Config.Desktop
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig
import XMonad.Layout.AvoidFloats

myTerminal      = "alacritty"
myFocusFollowsMouse = False
myClickJustFocuses = False

myModMask       = mod4Mask -- or mod1Mask for alt
myWorkspaces    = ["term", "browser", "discord", "games", "work", "6", "7", "8", "9"]

myLayout = avoidFloats tiled ||| avoidFloats (Mirror tiled) ||| avoidFloats Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = toRational (2 / (1 + sqrt 5 :: Double))

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

myManageHook = composeAll
	[ className =? "Firefox" 	--> doShift "browser"
	, className =? "discord" 	--> doShift "discord"
	, className =? "Alacritty" 	--> doShift "term"
	]

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((0, xK_Print), (spawn "flameshot full -c"))
    , ((0, xF86XK_AudioMute), spawn "amixer -D pulse set Master 1+ toggle")
    , ((0, xF86XK_AudioLowerVolume), spawn "amixer set 'Master' 2%-")
    , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set 'Master' 2%+")    
    , ((controlMask, xK_Print), (spawn "flameshot gui"))
    ]
------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
main = do
    xmonad $ ewmh $ desktopConfig {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = 0,
        modMask            = myModMask,
	manageHook	   = myManageHook,
        workspaces         = myWorkspaces,
        keys               = \c -> myKeys c `M.union` keys XMonad.def c,
        layoutHook         = desktopLayoutModifiers $ myLayout,
        handleEventHook    = fullscreenEventHook <+> handleEventHook desktopConfig,
	startupHook	   = do
		spawn "xmobar -x 0"
		spawn "firefox"
		spawn "/usr/bin/discord"
		spawn "alacritty"
		spawn "feh --bg-scale ~/.xmonad/wallpaper.jpg"
		setWMName "LG3D"
    }
