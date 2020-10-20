module Scene.Gameplay.Controls (input) where

import Components.Delayed.System (once_)
import Components.Fade.System (fadeOut)
import Components.Input.Schema (Bindings, SpecialKey(..), schema, keyDown, keyToggle)
import Scene.Gameplay.Controls.Types (Schema(..), PlayerAction(..), UiAction(..))

import World

import qualified Components.Input.System as Input
import qualified Scene.Outro.Transition as Outro
import qualified Scene.Gameplay.Transition as Gameplay

input :: Event -> SystemW ()
input = Input.handle
  [ playerSchema
  , uiSchema
  ]

playerSchema :: InputSchema
playerSchema = schema PlayerKeys playerActions playerBindings

playerActions :: Entity -> PlayerAction -> SystemW ()
playerActions pawn = \case
  RunLeft ->
    pawn $= Position (V2 (-100) 0)
  RunRight ->
    pawn $= Position (V2 100 0)
  Stop ->
    pawn $= Position (V2 0 0)

playerBindings :: Bindings PlayerAction
playerBindings =
  [ keyToggle KeyLeft  RunLeft  Stop
  , keyToggle KeyRight RunRight Stop
  ]

uiSchema :: InputSchema
uiSchema = schema GlobalUI uiActions uiBindings

uiActions :: Entity -> UiAction -> SystemW ()
uiActions _menu = \case
  Quit ->
    fadeOut 2.0 $ do
      Gameplay.leaveFor Outro
      once_ 2.0 $
        Outro.enterFrom Gameplay

uiBindings :: Bindings UiAction
uiBindings =
  [ keyDown KeyEsc Quit
  ]
