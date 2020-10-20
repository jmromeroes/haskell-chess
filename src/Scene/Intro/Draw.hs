module Scene.Intro.Draw
  ( draw
  , drawUI
  ) where

import Apecs.Gloss

import World
import Scene.Intro.Components (IntroMenu(..))

draw :: SystemW Picture
draw =
  pure .
    color green $
      text "Intro"

drawUI :: Screen -> SystemW Picture
drawUI Screen{} = do
  menu <- foldDraw drawMenu
  pure $ mconcat
    [ menu
    ]

drawMenu :: IntroMenu -> Picture
drawMenu IntroMenu{..} =
  color white $
    text $ show _introMenuCurrent
