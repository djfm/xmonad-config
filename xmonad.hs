import XMonad
import XMonad.Config.Xfce
import Data.Monoid
import XMonad.Util.EZConfig
import XMonad.Util.Run
import qualified XMonad.StackSet as W
import XMonad.Actions.GridSelect

range :: [Int]
range = [1..12]

workspaceKeys :: [KeySym]
workspaceKeys  = [0x26,0xe9,0x22,0x27,0x28,0x2d,0xe8,0x5f,0xe7,0xe0,0x29,0x3d]
aWorkspaceNames :: [String]
aWorkspaceNames     = map (\i -> 'a' : show i) range
bWorkspaceNames :: [String]
bWorkspaceNames     = map (\i -> 'b' : show i) range

alt :: KeyMask
alt = mod1Mask
win :: KeyMask
win = mod4Mask

main :: IO ()
main = xmonad $ xfceConfig {
  terminal            = "gnome-terminal",
  modMask             = alt,
  workspaces          = aWorkspaceNames ++ bWorkspaceNames,
  normalBorderColor   = "gray",
  focusedBorderColor  = "black",
  manageHook          = manageHook xfceConfig <+> myManageHook
} `additionalKeys` myKeyBindings

myKeyBindings :: [((KeyMask, KeySym), X ())]
myKeyBindings =
  makeWorkspacesKeyBinding alt aWorkspaceNames
  ++
  makeWorkspacesKeyBinding win bWorkspaceNames
  ++
  [((alt, xK_F2), safeSpawnProg "gmrun")]
  ++
  [((alt, xK_g), goToSelected def)]

makeWorkspacesKeyBinding :: KeyMask -> [WorkspaceId] -> [((KeyMask, KeySym), X ())]
makeWorkspacesKeyBinding myModMask workspaceNames = do
  (workspaceName, keySym) <- zip workspaceNames workspaceKeys
  (mask, action) <- [(0, W.greedyView), (shiftMask, W.shift)]
  return ((mask .|. myModMask, keySym), windows $ action workspaceName)

myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll [className =? "Do" --> doIgnore]
