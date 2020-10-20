-- | Single-file scene template. Consider using multi-file instead.

module Scene.Loading
  ( initialize
  , draw
  , drawUI
  , input
  , tick
  ) where

import Apecs (global, ($=))

import World

import qualified Scene.Intro.Transition as Intro

-- XXX: This one is special in having no access to drawable screen.
-- Set up loaders and pass over the controls.
initialize :: SystemW ()
initialize = do
  global $= Loading
  -- TODO: async loader example

draw :: SystemW Picture
draw =
  -- XXX: does NOT draw anythong until initialize finishes
  pure mempty

drawUI :: Screen -> SystemW Picture
drawUI Screen{} =
  -- TODO: draw splash screen / progress indicator
  -- XXX: does NOT draw anythong until initialize finishes
  pure mempty

input :: Event -> SystemW ()
input _event =
  pure ()

tick :: Float -> SystemW ()
tick _dt =
  -- TODO: check if something else needs loading
  Intro.enterFrom Loading
