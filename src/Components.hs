{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Components
( AllComps
, Time(..)
, WindowSize(..)
, MessageBit(..)
, MBit(..)
, Messages(..)
, GameState(..)
, Renderer(..)
, Textures(..)
, Fonts(..)
, GameMode(..)
, Tile(..)
, Color(..)
, GameMap(..)
, PlayerOne(..)
, PlayerOneReady(..)
, PlayerTwoReady(..)
, Relationships(..)
, Reticule(..)
, Position(..)
, CellRef(..)
, Examine(..)
, Sprite(..)
, FloatingTex(..)
) where

import Apecs
import SDL hiding (Vector, Renderer)
import qualified SDL
import qualified Data.HashMap.Strict as HM
import Data.Matrix

import Types
import Characters

-- Easy type for all non-global, non-player components
type AllComps = (Position, CellRef, Sprite, Character, FloatingTex)

-- Global component, exists outside of entities
-- Used for managing the passage of time
newtype Time = Time Double deriving Show
instance Semigroup Time where (<>) = mappend
instance Monoid Time where mempty = Time 0
instance Component Time where type Storage Time = Global Time

-- This component acts as a flag for when the player can move
newtype PlayerOneReady = PlayerOneReady Bool deriving Show
instance Semigroup PlayerOneReady where (<>) = mappend
instance Monoid PlayerOneReady where mempty = PlayerOneReady True
instance Component PlayerOneReady where type Storage PlayerOneReady = Global PlayerOneReady

-- This component acts as a flag for when the player can move
newtype PlayerTwoReady = PlayerTwoReady Bool deriving Show
instance Semigroup PlayerTwoReady where (<>) = mappend
instance Monoid PlayerTwoReady where mempty = PlayerTwoReady True
instance Component PlayerTwoReady where type Storage PlayerTwoReady = Global PlayerTwoReady

-- Global store of window size
newtype WindowSize = WindowSize (V2 Int) deriving Show
instance Semigroup WindowSize where (<>) = mappend
instance Monoid WindowSize where mempty = WindowSize (V2 0 0)
instance Component WindowSize where type Storage WindowSize = Global WindowSize

-- Global component used for reporting events
newtype Messages = Messages [[MBit]]
instance Semigroup Messages where (<>) = mappend
instance Monoid Messages where mempty = Messages []
instance Component Messages where type Storage Messages = Global Messages

-- Global store of renderer
newtype Renderer = Renderer (Maybe SDL.Renderer)
instance Component Renderer where type Storage Renderer = Global Renderer
instance Semigroup Renderer where (<>) = mappend
instance Monoid Renderer where mempty = Renderer Nothing

-- Global store of all textures
newtype Textures = Textures TextureMap
instance Component Textures where type Storage Textures = Global Textures
instance Semigroup Textures where (<>) = mappend
instance Monoid Textures where mempty = Textures HM.empty

-- Global store of all fonts
newtype Fonts = Fonts FontMap
instance Component Fonts where type Storage Fonts = Global Fonts
instance Semigroup Fonts where (<>) = mappend
instance Monoid Fonts where mempty = Fonts HM.empty

-- Global component used for changing gamestates
newtype GameState = GameState State
instance Semigroup GameState where (<>) = mappend
instance Monoid GameState where mempty = GameState $ Game Standard
instance Component GameState where type Storage GameState = Global GameState

-- Helpful data type for defining color of tiles and pieces
data Color = Black | White deriving (Show, Eq)

-- Global store of the current game map
data Tile = Tile Color deriving Show

newtype GameBoard = GameBoard (Matrix Tile)
instance Component GameBoard where type Storage GameBoard = Global GameBoard
instance Semigroup GameBoard where (<>) = mappend
instance Monoid GameBoard where mempty = GameBoard $ fromList 0 0 []

-- Unique component, either one or none exists
data PlayerOne = PlayerOne deriving Show
instance Component PlayerOne where type Storage PlayerOne = Unique PlayerOne

-- Unique component, either one or none exists
data PlayerTwo = PlayerTwo deriving Show
instance Component PlayerTwo where type Storage PlayerTwo = Unique PlayerTwo

-- All pieces components
data Queen = Queen Color deriving Show
instance Component Queen where type Storage Queen = Unique Queen Color

data King = King Color deriving Show
instance Component King where type Storage King = Unique King Color

data Rook = Rook Color deriving Show
instance Component Rook where type Storage Rook = Unique Rook Color

data Bishop = Bishop Color deriving Show
instance Component Bishop where type Storage Bishop = Unique Bishop Color

data Knight = Knight Color deriving Show
instance Component Knight where type Storage Knight = Unique Knight Color

data Pawn = Pawn Color deriving Show
instance Component Pawn where type Storage Pawn = Unique Pawn Color

-- Position of game entities
newtype Position = Position (V2 Double) deriving Show
instance Component Position where type Storage Position = Map Position

-- Cell reference of an entity
newtype CellRef = CellRef (V2 Int) deriving (Show, Eq)
instance Component CellRef where type Storage CellRef = Map CellRef

-- Texture coordinates of a sprite
data Sprite = Sprite String (Rectangle Int)
instance Component Sprite where type Storage Sprite = Map Sprite
