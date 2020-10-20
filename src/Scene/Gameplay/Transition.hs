module Scene.Gameplay.Transition
  ( enterFrom
  , leaveFor
  ) where

import Components.Fade.System (fadeIn)
import World
import Scene.Gameplay.Controls.Types (Schema(..))

import qualified Components.Input.System as Input
import qualified Utils.System as Utils

enterFrom :: Scene -> SystemW ()
enterFrom scene = do
  traceM $ "Gameplay: entering from " <> show scene

  global $= Gameplay
  Utils.cameraReset
  fadeIn 2.0

  Input.attach_ global GlobalUI

  pawn <- newEntity $
    Position 0

  Input.attach_ pawn PlayerKeys

leaveFor :: Scene -> SystemW ()
leaveFor scene = do
  traceM $ "Gameplay: leaving for " <> show scene

  cmap $ \Position{} -> Not @Position

  Input.destroyAll
