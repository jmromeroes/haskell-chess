module Scene.Gameplay.Draw
  ( draw
  , drawUI
  ) where

import Apecs.Gloss

import World

draw :: SystemW Picture
draw =
  foldDraw $ \(Position (V2 px py)) ->
    translate px py $
      color green $
        text "Game"

drawUI :: Screen -> SystemW Picture
drawUI Screen{..} =
  pure .
    translate 0 (negate $ fromIntegral _screenHeight / 2) .
      color red $
        text "Game UI"
