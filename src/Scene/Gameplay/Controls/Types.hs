module Scene.Gameplay.Controls.Types where

data Schema
  = PlayerKeys
  | GlobalUI
  deriving (Eq, Ord, Show, Enum, Bounded)

data PlayerAction
  = RunLeft
  | RunRight
  | Stop
  deriving (Eq, Ord, Show, Enum, Bounded)

data UiAction
  = Quit
  deriving (Show)
