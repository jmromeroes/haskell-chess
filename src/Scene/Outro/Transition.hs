module Scene.Outro.Transition
  ( enterFrom
  , leaveFor
  ) where

import Components.Fade.System (fadeIn)
import World
import Scene.Outro.Controls.Types (Schema(..))
-- import Scene.Outro.Components (OutroScore(..))

import qualified Components.Input.System as Input
import qualified Utils.System as Utils

enterFrom :: Scene -> SystemW ()
enterFrom scene = do
  traceM $ "Outro: entering from " <> show scene

  global $= Outro
  Utils.cameraReset
  fadeIn 2.0

  Input.attach_ global GlobalUI

leaveFor :: Scene -> SystemW ()
leaveFor scene = do
  traceM $ "Outro: leaving for " <> show scene

  Input.destroyAll
