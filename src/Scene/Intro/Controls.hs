module Scene.Intro.Controls (input) where

import Apecs
import Control.Lens

import Components.Delayed.System (once_)
import Components.Fade.System (fadeOut)
import Components.Input.Schema (Bindings, schema, SpecialKey(..), keyDown)
import Scene.Intro.Components
import Scene.Intro.Controls.Types (Schema(..), MenuAction(..))
import World

import qualified Components.Input.System as Input
import qualified Scene.Intro.Transition as Intro
import qualified Scene.Gameplay.Transition as Gameplay
import qualified Utils

input :: Event -> SystemW ()
input = Input.handle
  [ introSchema
  ]

introSchema :: InputSchema
introSchema = schema Menu uiActions uiBindings

uiActions :: Entity -> MenuAction -> SystemW ()
uiActions menu = \case
  MenuDown ->
    menu $~ introMenuCurrent %~ Utils.succB
  MenuUp ->
    menu $~ introMenuCurrent %~ Utils.predB
  MenuEnter ->
    get menu >>= \im ->
      case im ^. introMenuCurrent of
        Start -> do
          fadeOut 2.0 $ do
            Intro.leaveFor Gameplay
            once_ 2.0 $
              Gameplay.enterFrom Intro
        Quit ->
          fadeOut 1.0 $
            once_ 1.0 Utils.exit
  MenuQuit ->
    Utils.exit

uiBindings :: Bindings MenuAction
uiBindings =
  [ keyDown KeyDown  MenuDown
  , keyDown KeyUp    MenuUp
  , keyDown KeyEnter MenuEnter
  , keyDown KeyEsc   MenuQuit
  ]
