--------------------------------------------------------------------------------
-- File   : ~/.xmonad/xmonad.hs                                               --
-- Author : Xiaokui Shu <xiaokui.shu@ibm.com>                                 --
-- Xmonad : 0.13                                                              --
-- Update : 2018/05/16                                                        --
--                                                                            --
-- Multi-Screen (Multi-Head) Behavior                                         --
--   | start with only one screen       : ws#1 on screen 0                    --
--   | start with multi-screens         : ws#i on screen i | i <- [0..]       --
--   | rescreen from one to two screens : ws#g on the 2nd screen              --
--   where                                                                    --
--     workspaces = [ws#0 .. ws#9, ws#g]                                      --
--     shiftkeys  = [xK_0 .. xK_9, xK_grave]                                  --
--     ws#g (usually empty) is the reserved workspace for the 2nd screen      --
--                                                                            --
-- Additional Window Management                                               --
--   | modMask + g => list existing windows for switching                     --
--------------------------------------------------------------------------------

import Data.Ratio ((%))
import Control.Monad (when)
import Graphics.X11.ExtraTypes.XF86 ( xF86XK_MonBrightnessUp
                                    , xF86XK_MonBrightnessDown
                                    , xF86XK_AudioMute
                                    , xF86XK_AudioRaiseVolume
                                    , xF86XK_AudioLowerVolume
                                    , xF86XK_Display
                                    , xF86XK_ScreenSaver
                                    )

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Util.EZConfig (removeKeys, additionalKeys)
import XMonad.Layout.IndependentScreens (countScreens)
import XMonad.Actions.Warp (warpToScreen)
import XMonad.Actions.WindowBringer (gotoMenu)
import XMonad.Hooks.ManageHelpers ( composeOne
                                  , (-?>)
                                  , isFullscreen
                                  , doFullFloat
                                  )

--------------------------------------------------------------------------------
-- MAIN                                                                       --
--------------------------------------------------------------------------------

myModMask :: KeyMask
myModMask = mod1Mask

myTerminal :: String
myTerminal = "/usr/bin/urxvt"

main :: IO ()
main = do
    screenCnt <- countScreens
    xmonad $ def
        { terminal              = myTerminal
        , borderWidth           = 4
        , normalBorderColor     = "#222222"
        , focusedBorderColor    = "#535d6c"
        , modMask               = myModMask
        , workspaces            = myWorkspaces
        , startupHook           = myStartupHook screenCnt
        , manageHook            = myManageHook
        }
        `removeKeys`            defaultWorkspaceKeyMap
        `removeKeys`            defaultScreenKeyMap
        `removeKeys`            defaultComboMap
        `additionalKeys`        myWorkspaceKeyMap
        `additionalKeys`        myScreenKeyMap
        `additionalKeys`        myComboMap
        `additionalKeys`        myShortcutKeyMap

--------------------------------------------------------------------------------
-- Workspaces                                                                 --
--------------------------------------------------------------------------------

wsidToName :: (Integral i, Show i) => i -> String
wsidToName i
    | i >= 0 && i <= 9  = "ws#" ++ show i
    | i == 10           = "ws#g"
    | otherwise         = "ws#e"

-- special workspaces for new screen handling
wsZero :: String
wsZero  = wsidToName 0

wsOne :: String
wsOne   = wsidToName 1

wsGrave :: String
wsGrave = wsidToName 10

myWorkspaces :: [String]
myWorkspaces = map wsidToName [0 .. 10]

myWorkspaceKeys :: [KeySym]
myWorkspaceKeys = [xK_0 .. xK_9] ++ [xK_grave]

--------------------------------------------------------------------------------
-- Operation Changes                                                          --
--------------------------------------------------------------------------------

defaultWorkspaceKeyMap :: [(KeyMask, KeySym)]
defaultWorkspaceKeyMap =
    [(m .|. mod1Mask, n) | n <- [xK_1 .. xK_9], m <- [0, shiftMask]]

myWorkspaceKeyMap :: [((KeyMask, KeySym), X ())]
myWorkspaceKeyMap =
    [((m .|. myModMask, k), windows $ f i)
        | (i, k) <- zip myWorkspaces myWorkspaceKeys
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

defaultScreenKeyMap :: [(KeyMask, KeySym)]
defaultScreenKeyMap =
    [(m .|. mod1Mask, n) | n <- [xK_w, xK_e, xK_r], m <- [0, shiftMask]]

myScreenKeyMap :: [((KeyMask, KeySym), X ())]
myScreenKeyMap =
    [ ( (m .|. myModMask, key), do
        ws <- screenWorkspace sc
        whenJust ws (windows . f)
        warpToScreen sc (618%1000) (618%1000) -- additional mouse operation
      )
        | (key, sc) <- zip [xK_w, xK_e] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]

defaultComboMap :: [(KeyMask, KeySym)]
defaultComboMap =
    [ (mod1Mask                 , xK_Return)
    , (mod1Mask .|. shiftMask   , xK_Return)
    , (mod1Mask                 , xK_p)
    ]

myComboMap :: [((KeyMask, KeySym), X ())]
myComboMap =
    [ ((myModMask               , xK_Return), spawn myTerminal)
    , ((myModMask .|. shiftMask , xK_Return), windows W.swapMaster)
    , ((myModMask               , xK_r), spawn "dmenu_run")
    , ((myModMask               , xK_g), gotoMenu)
    ] 

--------------------------------------------------------------------------------
-- Additional Shortcuts                                                       --
--------------------------------------------------------------------------------

myShortcutKeyMap :: [((KeyMask, KeySym), X ())]
myShortcutKeyMap =
    [ ((0, xF86XK_MonBrightnessUp)      , spawn "xbacklight -inc 10")
    , ((0, xF86XK_MonBrightnessDown)    , spawn "xbacklight -dec 10")
    , ((0, xF86XK_AudioMute)            , spawn "amixer set Master toggle")
    , ((0, xF86XK_AudioRaiseVolume)     , spawn "amixer set Master 2%+")
    , ((0, xF86XK_AudioLowerVolume)     , spawn "amixer set Master 2%-")
    , ((0           , xF86XK_Display)   , rescreenExt)
    , ((shiftMask   , xF86XK_Display)   , rescreenMir)
    , ((0, xF86XK_ScreenSaver)          , spawn "i3lock -c 000000 -n")
    ]

--------------------------------------------------------------------------------
-- Multi-Screen                                                               --
--------------------------------------------------------------------------------

cmdXrandrExt :: String
cmdXrandrExt = "~/.xmonad/displaymgt.sh"

cmdXrandrMir :: String
cmdXrandrMir = "~/.xmonad/displaymgt.sh --mirror"

rescreenExt :: X ()
rescreenExt = do
    wsCurrent <- gets (W.currentTag . windowset)
    windows $ W.greedyView wsGrave
    windows $ W.greedyView wsCurrent
    spawn   $ cmdXrandrExt ++ ";" ++ cmdSetWallpaper

rescreenMir :: X ()
rescreenMir = spawn $ cmdXrandrMir ++ ";" ++ cmdSetWallpaper

--------------------------------------------------------------------------------
-- Composition Settings                                                       --
--------------------------------------------------------------------------------

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "MPlayer" --> doFloat
    ]
    <+>
    composeOne [ isFullscreen -?> doFullFloat ] -- Fix fullscreen issue

--------------------------------------------------------------------------------
-- Wallpaper                                                                  --
--------------------------------------------------------------------------------

cmdSetWallpaper :: String
cmdSetWallpaper = "feh --bg-fill ~/wallpaper/paine.jpg"

--------------------------------------------------------------------------------
-- Auto Startup                                                               --
--------------------------------------------------------------------------------

myStartupHook :: (Integral i) => i -> X ()
myStartupHook scrCnt = do
    spawn "xset s off -dpms"
    -- spawn "xbacklight -set 75"
    -- spawn "nm-applet"
    spawn cmdSetWallpaper
    when (scrCnt == 1) (windows $ W.greedyView wsOne)
