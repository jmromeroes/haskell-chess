module Config where

import Apecs.Gloss (Display(..))

-- * Gloss core

windowDisplay :: Display
windowDisplay = FullScreen

windowFPS :: Int
windowFPS = 60

windowScale :: Float
windowScale = 1.0
