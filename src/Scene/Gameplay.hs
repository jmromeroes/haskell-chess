-- | Aggregation module for split-module scene.

module Scene.Gameplay
  ( draw
  , drawUI
  , input
  , tick
  ) where

import Scene.Gameplay.Draw (draw, drawUI)
import Scene.Gameplay.Controls (input)
import Scene.Gameplay.Tick (tick)
