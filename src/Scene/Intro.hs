-- | Aggregation module for split-module scene.

module Scene.Intro
  ( draw
  , drawUI
  , input
  , tick
  ) where

import Scene.Intro.Draw (draw, drawUI)
import Scene.Intro.Controls (input)
import Scene.Intro.Tick (tick)
