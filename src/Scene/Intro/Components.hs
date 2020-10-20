{-# LANGUAGE TemplateHaskell #-}

module Scene.Intro.Components
  ( IntroMenu(..)
  , introMenuCurrent
  , introMenuTimer
  , MenuItem(..)
  ) where

import Apecs
import Control.Lens.TH (makeLenses)

data IntroMenu = IntroMenu
  { _introMenuCurrent :: MenuItem
  , _introMenuTimer   :: Float
  }
  deriving (Show)

instance Component IntroMenu where
  type Storage IntroMenu = Unique IntroMenu

data MenuItem
  = Start
  | Quit
  deriving (Eq, Ord, Show, Enum, Bounded)

makeLenses ''IntroMenu
