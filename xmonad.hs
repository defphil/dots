import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.Fullscreen (fullscreenFull, fullscreenSupport)
import XMonad.Layout.TwoPane (TwoPane(..))
import XMonad.Util.Themes
import XMonad.Layout.Tabbed
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO


myTabConfig =
  def
  { inactiveBorderColor = "#FF0000"
  , activeTextColor = "#00FF00"
  }


myLayoutHook =
  smartBorders . avoidStruts $ -- layouts begin below
  tiled ||| noBorders Full ||| tabbed shrinkText (theme robertTheme)
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100


main = do
    xmproc <- spawnPipe "/home/phil/.cabal/bin/xmobar /home/phil/.config/xmobar/xmobarrc"
    xmonad $ docks defaultConfig
      { manageHook = manageDocks <+> manageHook defaultConfig
      , layoutHook = myLayoutHook
      , terminal = "kitty"
      , logHook = dynamicLogWithPP xmobarPP
                  { ppOutput = hPutStrLn xmproc
                  , ppTitle = xmobarColor "green" "" . shorten 50
                  }
      , modMask = mod4Mask
      , normalBorderColor = "#202020"
      , focusedBorderColor = "#008080"
      , borderWidth = 1
      } `additionalKeys`
      [ ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
      , ((0, xK_Print), spawn "scrot")
      , ((mod4Mask, xK_z), spawn "i3lock -i /home/phil/Pictures/wp.png")
      , ((mod4Mask, xK_p), spawn "dmenu_run -nb black -fn 'Terminus-9'")
      , ((0                     , 0x1008FF11), spawn "amixer -q sset Master 2%-")
      , ((0                     , 0x1008FF13), spawn "amixer -q sset Master 2%+")
      , ((0                     , 0x1008FF12), spawn "amixer set Master toggle")
      , ((0 , 0x1008FF02), spawn "lux -a 20%")
      , ((0 , 0x1008FF03), spawn "lux -s 20%")
      ]
