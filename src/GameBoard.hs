module GameBoard
( getTile
, generateEmptyBoard
) where

import SDL hiding (Vector)

import Data.Matrix
import Data.Vector(imap, ifoldl, toList)

import Common
import Components

-- Matrix accessor with V2 support
getTile :: Matrix Tile -> V2 Int -> Maybe Tile
getTile = getItem

-- Iterate through the matrix via folding
foldMatrix :: Matrix a -> (b -> V2 Int -> a -> b) -> b -> b
foldMatrix m func s = ifoldl (\p i n -> func p (pos i) n) s mat
  where pos i = let c = ncols m in V2 (i `mod` c) (i `div` c)
        mat = getMatrixAsVector m

-- Iterate through the matrix mapping 1 to 1
mapMatrix :: Matrix a -> (V2 Int -> a -> b) -> Matrix b
mapMatrix m func = Data.Matrix.fromList (nrows m) (ncols m) vec
  where vec = Data.Vector.toList $ imap (\i n -> func (pos i) n) mat
        pos i = let c = ncols m in V2 (i `mod` c) (i `div` c)
        mat = getMatrixAsVector m

-- Check the tile type according to the row and col
getCellType :: Int -> Int -> Tile
getCellType x y
  | y `mod` 2 == 0 =
      case x `mod` 2 == 0 of
        True -> Black
        False -> White
  | otherwise =
      case x `mod` 2 == 0 of
        True -> White
        Flase -> Black

-- Identity map
generateEmptyBoard :: V2 Int -> Matrix Tile
generateEmptyBoard (V2 w h) = matrix w h (\(x, y) -> getCellType x y)
