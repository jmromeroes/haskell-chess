module Components.Fade.System
  ( fadeIn
  , fadeOut
  , tick
  , tickFadeIn
  , tickFadeOut
  ) where

import Apecs -- (SystemT)
import Control.Lens
import Control.Monad

import Components.Fade.Types

fadeIn
  :: (Has world IO FadeIn, Has world IO FadeOut)
  => Float -> SystemT world IO ()
fadeIn limit = do
  global $= FadeIn
    { _fadeInTimer = 0
    , _fadeInLimit = limit
    }
  destroy global $ Proxy @FadeOut

fadeOut
  :: (Has world IO FadeIn, Has world IO FadeOut)
  => Float
  -> SystemT world IO ()
  -> SystemT world IO ()
fadeOut limit action =
  get global >>= \case
    Just FadeOut{} ->
      pure ()
    Nothing -> do
      -- TODO: intercept fade-in timer
      destroy global $ Proxy @FadeIn
      fadeOut' limit
      action

fadeOut' :: Has world IO FadeOut => Float -> SystemT world IO ()
fadeOut' limit =
  global $= FadeOut
    { _fadeOutTimer = 0
    , _fadeOutLimit = limit
    }

tick
  :: (Has world IO FadeIn, Has world IO FadeOut)
  => Float -> SystemT world IO ()
tick dt = do
  tickFadeIn dt
  tickFadeOut dt

tickFadeIn :: (Has world IO FadeIn) => Float -> SystemT world IO ()
tickFadeIn dt = do
  cmap $ \FadeIn{..} ->
    let
      timer' = _fadeInTimer + dt
    in
      if timer' >= _fadeInLimit then
        Nothing
      else
        Just FadeIn
          { _fadeInTimer = timer'
          , ..
          }

tickFadeOut :: (Has world IO FadeOut) => Float -> SystemT world IO ()
tickFadeOut dt = do
  cmapM_ $ \(FadeOut{..}, e) ->
    when (_fadeOutTimer /= _fadeOutLimit) $ do
      let timer' = min (_fadeOutTimer + dt) _fadeOutLimit
      e $~ fadeOutTimer .~ timer'
