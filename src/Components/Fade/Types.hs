{-# LANGUAGE TemplateHaskell #-}

module Components.Fade.Types
  ( FadeIn(..)
  , fadeInTimer
  , fadeInLimit

  , FadeOut(..)
  , fadeOutTimer
  , fadeOutLimit
  ) where

import Apecs (Component, Storage, Unique)
import Control.Lens.TH (makeLenses)

-- | Linear countdown toward limit then disappears.
data FadeIn = FadeIn
  { _fadeInTimer :: Float
  , _fadeInLimit :: Float
  }
  deriving (Show)

instance Component FadeIn where
  type Storage FadeIn = Unique FadeIn

-- | Linear countdown toward limit and stays.
--
-- Used for preventing further action during the transition.
data FadeOut = FadeOut
  { _fadeOutTimer :: Float
  , _fadeOutLimit :: Float
  }
  deriving (Show)

instance Component FadeOut where
  type Storage FadeOut = Unique FadeOut

makeLenses ''FadeIn
makeLenses ''FadeOut
