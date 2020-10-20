module Utils.System
  ( cameraReset
  ) where

import World

cameraReset :: SystemW ()
cameraReset = global $= Camera
  { camOffset = 0
  , camScale  = 1.0
  }
