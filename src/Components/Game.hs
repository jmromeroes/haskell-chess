{-# LANGUAGE TemplateHaskell #-}

module Components.Game
  ( Scene(..)
  , Player(..)
  , Score(..)
  , scorePoints
  ) where

import Apecs (Component(..), Global, Map)
import Control.Lens (makeLenses)

-- | The "router" of the game.
-- Handlers and rendering switch on this. Add more as you go.
data Scene
  = Loading
  | Intro
  | Gameplay
  | Outro
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Semigroup Scene where
  _a <> b = b

instance Monoid Scene where
  mempty = minBound

instance Component Scene where
  type Storage Scene = Global Scene

-- | Player-related entities.
--
-- Usually their "pawn", but can be attached to things like projectiles, score etc.
data Player = Player
  deriving (Show)

instance Component Player where
  type Storage Player = Map Player

-- | Game score. Can be global
data Score = Score
  { _scorePoints :: Int
  } deriving (Show)

instance Semigroup Score where
  _a <> b = b

instance Monoid Score where
  mempty = Score 0

instance Component Score where
  type Storage Score = Map Score

makeLenses ''Score
