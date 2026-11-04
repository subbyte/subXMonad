--------------------------------------------------------------------------------
-- File   : ~/.xmonad/xmonad.hs                                               --
-- Author : Xiaokui Shu                                                       --
-- Xmonad : 0.18.1                                                            --
-- Update : 2026/11/02                                                        --
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
--   | modMask + c => start a floating console                                --
--                                                                            --
-- Additional Shortcut                                                        --
--   |         modMask + r              => dmenu                              --
--   |         modMask + g              => gotoMenu                           --
--   |         xF86XK_Display           => detect/set external minitors       --
--   | Shfit + xF86XK_Display           => mirror external minitors           --
--   | Ctrl  + xF86XK_Display           => turn off external minitors         --
--   |         xF86XK_Time              => show time and date                 --
--   |         xF86XK_AudioMute         => mute/unmute                        --
--   |         xF86XK_AudioRaiseVolume  => unmute & volume up                 --
--   |         xF86XK_AudioLowerVolume  => unmute & volume down               --
--   |         xF86XK_MonBrightnessUp   => brightness up                      --
--   |         xF86XK_MonBrightnessDown => brightness down                    --
--                                                                            --
-- Border Customization For Each Window                                       --
--   | className =? "firefox" --> defineBorderWidth 0                         --
--   | className =? "Chromium" --> defineBorderWidth 0                        --
--------------------------------------------------------------------------------

import Control.Concurrent (threadDelay)
import Control.Monad (void, when)
import Data.Map (Map)
import Data.Monoid (All)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)
import Graphics.X11.ExtraTypes.XF86 ( xF86XK_MonBrightnessUp
                                    , xF86XK_MonBrightnessDown
                                    , xF86XK_AudioMute
                                    , xF86XK_AudioRaiseVolume
                                    , xF86XK_AudioLowerVolume
                                    , xF86XK_Display
                                    , xF86XK_Time
                                    )
import System.FilePath ((</>))
import XMonad
import XMonad.Actions.Warp (warpToScreen)
import XMonad.Actions.WindowBringer (gotoMenu)
import XMonad.Core (cfgDir)
import XMonad.Hooks.BorderPerWindow (actionQueue, defineBorderWidth)
import XMonad.Hooks.ManageHelpers (doRectFloat)
import XMonad.Hooks.OnPropertyChange (onXPropertyChange)
import XMonad.Layout.IndependentScreens (countScreens)
import qualified XMonad.StackSet as W
import XMonad.Util.CustomKeys (customKeys)
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Run (runProcessWithInput, safeSpawn)
import XMonad.Util.Timer (TimerId, handleTimer, startTimer)
import XMonad.Util.WindowProperties (Property (Role), propertyToQuery)
import XMonad.Util.XUtils ( showSimpleWindow
                          , deleteWindow
                          , WindowConfig(WindowConfig)
                          , WindowRect(CenterWindow)
                          )

--------------------------------------------------------------------------------
-- MAIN                                                                       --
--------------------------------------------------------------------------------

myModMask :: KeyMask
myModMask = mod1Mask

myTerminal :: String
myTerminal = "/usr/bin/alacritty"

wallpaperFile :: String
wallpaperFile = "wallpaper.jpg"

displaymgtScript :: String
displaymgtScript = "displaymgt.sh"

volumectlScript :: String
volumectlScript = "volumectl.sh"

fconsoleName :: String
fconsoleName = "fconsole"

main :: IO ()
main = do
    screenCnt <- countScreens
    xmonad $ actionQueue $ def
        { normalBorderColor     = "#222222"
        , focusedBorderColor    = "#535d6c"
        , terminal              = myTerminal
        , layoutHook            = myLayout
        , manageHook            = myManageHook
        , handleEventHook       = osdTimerHook <+> myDynamicManageHook
        , workspaces            = myWorkspaces
        , modMask               = myModMask
        , keys                  = myKeyMaps
        , borderWidth           = 4
        , startupHook           = myStartupHook screenCnt
        }

--------------------------------------------------------------------------------
-- Terminal                                                                   --
--------------------------------------------------------------------------------

execTerminal :: X ()
execTerminal = do
    safeSpawn myTerminal []

execFloatingConsole :: X ()
execFloatingConsole = do
    safeSpawn myTerminal $
        [ "--title"
        , fconsoleName
        , "-e"
        , "bash"
        , "--rcfile"
        , "~/.bashrc_console"
        ]

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
-- Layout Settings                                                            --
--------------------------------------------------------------------------------

myLayout = Tall 1 (3/100) (1/2) ||| Mirror (Tall 1 (3/100) (1/2))

--------------------------------------------------------------------------------
-- Key Map Changes                                                          --
--------------------------------------------------------------------------------

myKeyMaps :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeyMaps = customKeys (\_ -> removedKeyMaps) (\_ -> insertedKeyMaps)

removedKeyMaps :: [(KeyMask, KeySym)]
removedKeyMaps =  defaultWorkspaceKeyMap
               ++ defaultScreenKeyMap
               ++ defaultOpKeyMap

insertedKeyMaps :: [((KeyMask, KeySym), X ())]
insertedKeyMaps =  myWorkspaceKeyMap
                ++ myScreenKeyMap
                ++ myOpKeyMap
                ++ myShortcutKeyMap

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
        warpToScreen sc 0.618 0.618 -- additional mouse operation
      )
        | (key, sc) <- zip [xK_e, xK_w] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]

defaultOpKeyMap :: [(KeyMask, KeySym)]
defaultOpKeyMap =
    [ (mod1Mask                 , xK_Return)
    , (mod1Mask .|. shiftMask   , xK_Return)
    , (mod1Mask                 , xK_p)
    ]

myOpKeyMap :: [((KeyMask, KeySym), X ())]
myOpKeyMap =
    [ ((myModMask               , xK_Return), execTerminal)
    , ((myModMask .|. shiftMask , xK_Return), windows W.swapMaster)
    , ((myModMask               , xK_c), execFloatingConsole)
    , ((myModMask               , xK_r), safeSpawn "dmenu_run" [])
    , ((myModMask               , xK_g), gotoMenu)
    ] 

myShortcutKeyMap :: [((KeyMask, KeySym), X ())]
myShortcutKeyMap =
    [ ((noModMask, xF86XK_MonBrightnessUp)  , safeSpawn "xbacklight" ["-inc", "10"])
    , ((noModMask, xF86XK_MonBrightnessDown), safeSpawn "xbacklight" ["-dec", "10"])
    , ((noModMask, xF86XK_AudioMute)        , volumeControl VolMute)
    , ((noModMask, xF86XK_AudioRaiseVolume) , volumeControl VolUp)
    , ((noModMask, xF86XK_AudioLowerVolume) , volumeControl VolDown)
    , ((noModMask, xF86XK_Display)          , rescreenExt)
    , ((shiftMask, xF86XK_Display)          , rescreenMir)
    , ((controlMask, xF86XK_Display)        , rescreenNoE)
    , ((noModMask, xF86XK_Time)             , fclock)
    , ((noModMask, xK_Super_L)              , fclock)
    ]

--------------------------------------------------------------------------------
-- Composition Settings                                                       --
--------------------------------------------------------------------------------

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "MPlayer" --> doFloat

    -- floating console
    , title =? fconsoleName
        --> doRectFloat (W.RationalRect 0.618 0.68 0.26 0.18)
 
    -- resize and float all dialog window
    , propertyToQuery (Role "GtkFileChooserDialog")
        --> doRectFloat (W.RationalRect 0.25 0.25 0.5 0.5)
    
    -- no border for following windows
    -- use `xprop` to find window className
    , className =? "firefox" --> defineBorderWidth 0
    , className =? "Firefox" --> defineBorderWidth 0
    , className =? "Google-chrome" --> defineBorderWidth 0
    , className =? "Microsoft-edge" --> defineBorderWidth 0
    , className =? "Chromium" --> defineBorderWidth 0
    , className =? "Chromium-browser" --> defineBorderWidth 0
    , className =? "Opera" --> defineBorderWidth 0
    , className =? "mpv" --> defineBorderWidth 0
    , className =? "feh" --> defineBorderWidth 0
    , className =? "VirtualBox Manager" --> defineBorderWidth 0
    , title     =? fconsoleName --> defineBorderWidth 2
    ]

-- hack to resolve VirtualBox bug (#18042) on delayed WM_NAME assignment
myDynamicManageHook :: Event -> X All
myDynamicManageHook = onXPropertyChange "WM_NAME"
    ( className =? "VirtualBox Machine" --> defineBorderWidth 0 )

--------------------------------------------------------------------------------
-- On-Screen Display State                                                    --
--------------------------------------------------------------------------------

data OSDState = OSDState (Maybe (TimerId, Window))

instance ExtensionClass OSDState where
    initialValue = OSDState Nothing

osdTimerHook :: Event -> X All
osdTimerHook e = do
    OSDState mActive <- XS.get
    whenJust mActive $ \(tid, win) -> 
        void $ handleTimer tid e $ do
            deleteWindow win
            XS.put $ OSDState Nothing
            return Nothing
    return mempty

flashOSD :: WindowConfig -> String -> Rational -> X ()
flashOSD config message duration = do
    OSDState mActive <- XS.get
    whenJust mActive $ \(_, oldWin) -> deleteWindow oldWin
    win <- showSimpleWindow config [message]
    tid <- startTimer duration
    XS.put $ OSDState $ Just (tid, win)

--------------------------------------------------------------------------------
-- Floating Clock                                                             --
--------------------------------------------------------------------------------

fclock :: X ()
fclock = do
    time <- liftIO clockString
    flashOSD clockWindowConfig time 1
    where
      clockWindowConfig =
          WindowConfig "xft:monospace-18" "black" "white" CenterWindow

clockString :: IO String
clockString = do
    t <- getZonedTime
    let val = formatTime defaultTimeLocale " %r | %b %d " t
    return val

--------------------------------------------------------------------------------
-- Volume Control                                                             --
--------------------------------------------------------------------------------

volumectlCmd :: X String
volumectlCmd = do
    configDir <- asks (cfgDir . directories)
    return (configDir </> volumectlScript)

data VolAction = VolUp | VolDown | VolMute

volumeControl :: VolAction -> X ()
volumeControl action = do
    cmd <- volumectlCmd
    volume <- runProcessWithInput cmd [arg] ""
    flashOSD volumeWindowConfig (" " ++ volume ++ " ") 0.4
    where
      arg = case action of
        VolUp   -> "+"
        VolDown -> "-"
        VolMute -> "m"
      volumeWindowConfig =
          WindowConfig "xft:monospace-18" "#cccccc" "#333333" CenterWindow

--------------------------------------------------------------------------------
-- Multi-Screen                                                               --
--------------------------------------------------------------------------------

displaymgtCmd :: X String
displaymgtCmd = do
    configDir <- asks (cfgDir . directories)
    return (configDir </> displaymgtScript)

rescreenExt :: X ()
rescreenExt = do
    wsCurrent <- gets (W.currentTag . windowset)

    -- prepare workspace order so the new screen gets wsGrave
    windows $ W.greedyView wsGrave
    windows $ W.greedyView wsCurrent

    cmd <- displaymgtCmd
    safeSpawn cmd []
    setWallPaper

rescreenMir :: X ()
rescreenMir = do
    cmd <- displaymgtCmd
    safeSpawn cmd ["--mirror"]
    setWallPaper

rescreenNoE :: X ()
rescreenNoE = do
    cmd <- displaymgtCmd
    safeSpawn cmd ["--noext"]
    setWallPaper

--------------------------------------------------------------------------------
-- Wallpaper                                                                  --
--------------------------------------------------------------------------------

setWallPaper :: X ()
setWallPaper = do
    configDir <- asks (cfgDir . directories)
    safeSpawn "feh" ["--bg-fill", configDir </> wallpaperFile]

--------------------------------------------------------------------------------
-- Auto Startup                                                               --
--------------------------------------------------------------------------------

myStartupHook :: (Integral i) => i -> X ()
myStartupHook scrCnt = do
    when (scrCnt == 1) (windows $ W.greedyView wsOne)
    -- when (scrCnt == 2) rescreenNoE
    -- windows $ W.greedyView wsOne

    -- liftIO $ threadDelay 1000000
    setWallPaper
