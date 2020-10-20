module Utils.Play
  ( playUI
  ) where

import Apecs
import Apecs.Core
import Apecs.Gloss

playUI
  :: ( Has w IO Camera
     , Has w IO screen
     , ExplGet IO (Storage screen)
     )
  => Display                      -- ^ Display mode
  -> Color                        -- ^ Background color
  -> Int                          -- ^ Desired FPS
  -> System w Picture             -- ^ Drawing function (camera coords)
  -> (screen -> System w Picture) -- ^ UI Drawing function (window coords)
  -> (Event -> System w ())       -- ^ Event handling function
  -> (Float  -> System w ())      -- ^ Stepping function, with a time delta argument.
  -> System w ()
playUI disp col fps draw drawUI handle step = do
  w <- ask
  liftIO $ playIO disp col fps w draw' handle' step'
    where
      handle' event =
        runSystem $
          handle event >> ask

      step' dT =
        runSystem $
          step dT >> ask

      draw' = runSystem $ do
        (cam, screen) <- get global
        scene <- draw
        ui <- drawUI screen
        pure $ cameraTransform cam scene <> ui
