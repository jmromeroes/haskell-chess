{-# LANGUAGE TemplateHaskell #-}

module Components.Delayed.Types where

import Apecs
import Data.Typeable (Typeable)

import Control.Lens (makeLenses)

data Finish
  = Cleanup
  | NoCleanup
  deriving (Eq, Ord, Show, Enum, Bounded)

data Delayed = forall w . Typeable w => Delayed
  { _delayedTime    :: Float
  , _delayedCancel  :: Bool
  , _delayedFinish  :: Finish
  , _delayedAction  :: SystemT w IO ()
  , _delayedCleanup :: SystemT w IO ()
  }

instance Component Delayed where
  type Storage Delayed = Map Delayed

makeLenses ''Delayed
