{-# LANGUAGE TemplateHaskell #-}

-- | A place for every possible component in the world. And the world.

module World
  ( SystemW
  , InputSchema

  -- * Generated by Apecs
  , World(..)
  , initWorld

  -- * Convenience re-exports, do not abuse
  , module Components.Common
  , module Components.Game

 -- * Kinda prelude

 -- ** Apecs bits
  , Entity(..)
  , Not(..)
  , cfold
  , cfoldM
  , cfoldM_
  , cmap
  , cmapM
  , cmapM_
  , get
  , global
  , destroy
  , newEntity
  , ($=)
  , ($~)

  -- ** Lens
  , module Control.Lens

  -- ** Gloss
  , Event
  , Picture
  , Point

  -- ** Base
  , Proxy(..)
  , traceM
  , traceMarkerW

  -- ** Linear
  , V2(..)
  ) where

import Apecs
import Apecs.Gloss (Event, Picture, Point)
import Control.Lens ((^.), (.~), (%~), (+~), (-~), (*~))
import Linear.V2 (V2(..))
import Debug.Trace (traceM, traceMarkerIO)

import Components.Common
import Components.Game

import qualified Components.Delayed.Types as Delayed
import qualified Components.Fade.Types as Fade
import qualified Components.Input.Types as Input
import qualified Scene.Intro.Components as Intro

makeWorld "World"
  [ ''Camera
  , ''Screen
  , ''Cursor
  , ''DragStart

  , ''Scene

  , ''Player
  , ''Score

  , ''Position
  , ''Velocity
  , ''Direction
  , ''Turn

  , ''Delayed.Delayed
  , ''Fade.FadeIn
  , ''Fade.FadeOut
  , ''Input.InputControls

  , ''Intro.IntroMenu
  ]

type SystemW a = SystemT World IO a

type InputSchema = Input.Schema World

traceMarkerW :: String -> SystemW ()
traceMarkerW = liftIO . traceMarkerIO
