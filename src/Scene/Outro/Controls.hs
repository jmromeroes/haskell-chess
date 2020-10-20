module Scene.Outro.Controls (input) where

import Components.Delayed.System (once_)
import Components.Fade.System (fadeOut)
import Components.Input.Schema (Bindings, schema, SpecialKey(..), keyDown)
import Scene.Outro.Controls.Types (Schema(..), UiAction(..))
import World

import qualified Components.Input.System as Input
import qualified Scene.Intro.Transition as Intro
import qualified Scene.Outro.Transition as Outro

input :: Event -> SystemW ()
input = Input.handle
  [ outroSchema
  ]

outroSchema :: InputSchema
outroSchema = schema GlobalUI uiActions uiBindings

uiActions :: Entity -> UiAction -> SystemW ()
uiActions _menu = \case
  ExitToMenu ->
    fadeOut 2.0 $ do
      Outro.leaveFor Intro
      once_ 2.0 $
        Intro.enterFrom Outro

uiBindings :: Bindings UiAction
uiBindings =
  [ keyDown KeyEsc ExitToMenu
  ]
