module Utils
  ( exit

  , succB
  , predB
  , trimming
  ) where

import Apecs (liftIO)
import Control.Lens (Lens', lens)
import Debug.Trace (traceM)
import System.Exit (exitSuccess)

import World

exit :: SystemW ()
exit = do
  traceM "Exit"
  liftIO exitSuccess


succB :: (Eq a, Bounded a, Enum a) => a -> a
succB x
  | x == maxBound = x
  | otherwise = succ x

predB :: (Eq a, Bounded a, Enum a) => a -> a
predB x
  | x == minBound = x
  | otherwise = pred x

trimming :: (Ord t) => t -> t -> Lens' t t
trimming minB maxB = lens id setter
  where
    setter _old new
      | new > maxB = maxB
      | new < minB = minB
      | otherwise = new
