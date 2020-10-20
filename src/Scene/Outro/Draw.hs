module Scene.Outro.Draw
  ( draw
  , drawUI
  ) where

import Apecs.Gloss

import World

draw :: SystemW Picture
draw =
  pure .
    color green $
      text "Outro"

drawUI :: Screen -> SystemW Picture
drawUI Screen{} =
  pure .
    color red $
      text "Outro UI"
