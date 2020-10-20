module Scene.Intro.Transition
  ( enterFrom
  , leaveFor
  ) where

import Components.Fade.System (fadeIn)
import World
import Scene.Intro.Controls.Types (Schema(..))
import Scene.Intro.Components (IntroMenu(..))

import qualified Components.Input.System as Input
import qualified Utils.System as Utils

enterFrom :: Scene -> SystemW ()
enterFrom scene = do
  traceM $ "Intro: entering from " <> show scene

  global $= Intro
  Utils.cameraReset
  fadeIn 2.0

  global $= IntroMenu
    { _introMenuCurrent = minBound
    , _introMenuTimer   = 0
    }
  Input.attach_ global Menu

leaveFor :: Scene -> SystemW ()
leaveFor scene = do
  traceM $ "Intro: leaving for " <> show scene

  Input.destroyAll
  destroy global $ Proxy @IntroMenu
