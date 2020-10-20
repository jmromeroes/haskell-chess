{-# LANGUAGE TemplateHaskell #-}

module Components.Common
  ( Camera(..)
  , Screen(..)
  , screenWidth
  , screenHeight
  , Cursor(..)
  , DragStart(..)

  , Position(..)
  , Velocity(..)
  , Direction(..)
  , Turn(..)

  , V2(..)
  ) where

import Apecs (Component(..), Global, Unique, Map)
import Apecs.Gloss (Camera(..))
import Control.Lens (makeLenses)
import Linear.V2 (V2(..))

-- * UI and controls

-- | Resize handler store.
data Screen = Screen
  { _screenWidth  :: Int
  , _screenHeight :: Int
  } deriving (Show)

makeLenses ''Screen

instance Semigroup Screen where
  _a <> b = b

instance Monoid Screen where
  mempty = Screen 0 0

instance Component Screen where
  type Storage Screen = Global Screen

-- | Cursor position in scene coordinates.
--
-- "Unique" instead of "Global" because it may be hidden (e.g. coming out of some region).
data Cursor = Cursor (V2 Float)
  deriving (Show)

instance Component Cursor where
  type Storage Cursor = Unique Cursor

-- | Origin point of drag-n-drop action.
newtype DragStart = DragStart (V2 Float)
  deriving (Show)

instance Component DragStart where
  type Storage DragStart = Unique DragStart

-- * Common aspects

-- | Position in scene coordinates.
newtype Position = Position (V2 Float)
  deriving (Eq, Ord, Show)

instance Component Position where
  type Storage Position = Map Position

-- | Velocity in scene distance per second.
newtype Velocity = Velocity (V2 Float)
  deriving (Eq, Ord, Show)

instance Component Velocity where
  type Storage Velocity = Map Velocity

-- | Rotation in degrees.
--
-- Clockwise: 0..180
-- Counter-clockwise: 0..-180
newtype Direction = Direction Float
  deriving (Eq, Ord, Show)

instance Component Direction where
  type Storage Direction = Map Direction

-- | Rotation velocity in degrees per second.
newtype Turn = Turn Float
  deriving (Eq, Ord, Show)

instance Component Turn where
  type Storage Turn = Map Turn
