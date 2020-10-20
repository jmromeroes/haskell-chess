-- | Aggregation module for split-module scene.

module Scene.Outro
  ( draw
  , drawUI
  , input
  , tick
  ) where

import Scene.Outro.Draw (draw, drawUI)
import Scene.Outro.Controls (input)
import Scene.Outro.Tick (tick)
