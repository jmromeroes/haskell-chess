module Scene.Intro.Tick (tick) where

import World
import Scene.Intro.Components (introMenuTimer)

import qualified Config

tick :: Float -> SystemW ()
tick dt = do
  cmap $ introMenuTimer +~ dt

  -- For example purposes only.
  -- The scene camera shifts to the right, while the UI stays in place.
  global $~ \Camera{camOffset=V2 x y, camScale} ->
    Camera
      { camOffset =
          V2 (x + dt * fromIntegral Config.windowFPS) y
      , ..
      }
