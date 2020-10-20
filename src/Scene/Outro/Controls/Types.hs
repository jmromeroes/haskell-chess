module Scene.Outro.Controls.Types where

data Schema
  = GlobalUI
  deriving (Eq, Ord, Show, Enum, Bounded)

data UiAction
  = ExitToMenu
