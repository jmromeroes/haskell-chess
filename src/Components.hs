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
, GameMap(..)
, PlayerOne(..)
, PlayerOneReady(..)
, PlayerTwoReady(..)
, Relationships(..)
, Reticule(..)
, Position(..)
, CellRef(..)
, Character(..)
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

-- Global store of the current game map
data Tile = Black | White deriving (Show, Eq)
newtype GameBoard = GameBoar%d (Matrix Tile)
instance Component GameBoard where type Storage GameBoard = Global GameBoard
instance Semigroup GameBoard where (<>) = mappend
instance Monoid GameBoard where mempty = GameBoard $ fromList 0 0 []

-- Unique component, either one or none exists
data PlayerOne = PlayerOne deriving Show
instance Component PlayerOne where type Storage PlayerOne = Unique PlayerOne

-- Unique component, either one or none exists
data PlayerTwo = PlayerTwo deriving Show
instance Component PlayerTwo where type Storage PlayerTwo = Unique PlayerTwo

-- All white pieces components
data WhiteQueen = WhiteQueen deriving Show
instance Component WhiteQueen where type Storage WhiteQueen = Unique WhiteQueen

data WhiteKing = WhiteKing deriving Show
instance Component WhiteKing where type Storage WhiteKing = Unique WhiteKing

data WhiteRook = WhiteRook deriving Show
instance Component WhiteRook where type Storage WhiteRook = Map WhiteRook

data WhiteBishop = WhiteBishop deriving Show
instance Component WhiteBishop where type Storage WhiteBishop = Map WhiteBishop

data WhiteKnight = WhiteKnight deriving Show
instance Component WhiteKnight where type Storage WhiteKnight = Map WhiteKnight

data WhitePawn = WhitePawn deriving Show
instance Component WhitePawn where type Storage WhitePawn = Map WhitePawn

-- All black pieces compoenents
data BlackQueen = BlackQueen deriving Show
instance Component BlackQueen where type Storage BlackQueen = Unique BlackQueen

data BlackKing = BlackKing deriving Show
instance Component BlackKing where type Storage BlackKing = Unique BlackKing

data BlackRook = BlackRook deriving Show
instance Component BlackRook where type Storage BlackRook = Map BlackRook

data BlackBishop = BlackBishop deriving Show
instance Component BlackBishop where type Storage BlackBishop = Map BlackBishop

data BlackKnight = BlackKnight deriving Show
instance Component BlackKnight where type Storage BlackKnight = Map BlackKnight

data BlackPawn = BlackPawn deriving Show
instance Component BlackPawn where type Storage BlackPawn = Map BlackPawn

-- Position of game entities
newtype Position = Position (V2 Double) deriving Show
instance Component Position where type Storage Position = Map Position

-- Cell reference of an entity
newtype CellRef = CellRef (V2 Int) deriving (Show, Eq)
instance Component CellRef where type Storage CellRef = Map CellRef

-- Texture coordinates of a sprite
data Sprite = Sprite String (Rectangle Int)
instance Component Sprite where type Storage Sprite = Map Sprite
