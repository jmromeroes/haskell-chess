module Components.Fade.Draw
  ( drawUI
  ) where

import Apecs.Gloss

import World (Screen(..), SystemW)
import Components.Fade.Types (FadeIn(..), FadeOut(..))

drawUI :: Screen -> SystemW Picture
drawUI Screen{..} = do
  fin <- foldDraw $ \FadeIn{..} ->
    shade $ 1.0 - _fadeInTimer / _fadeInLimit

  fout <- foldDraw $ \FadeOut{..} ->
    shade $ _fadeOutTimer / _fadeOutLimit

  pure (fin <> fout)
  where
    shade alpha =
      color (withAlpha alpha black) $
        rectangleSolid
          (fromIntegral _screenWidth)
          (fromIntegral _screenHeight)
