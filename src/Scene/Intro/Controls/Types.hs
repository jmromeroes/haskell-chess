module Scene.Intro.Controls.Types where

data Schema
  = Menu
  deriving (Eq, Ord, Show, Enum, Bounded)

data MenuAction
  = MenuUp
  | MenuDown
  | MenuEnter
  | MenuQuit
  deriving (Show)
