module Main where

import Apecs (get, global, runWith)
import Apecs.Gloss (Event(..), black)

import Utils.Play (playUI)
import World

import qualified Components.Delayed.System as Delayed
import qualified Components.Fade.System as Fade
import qualified Components.Fade.Draw as Fade

import qualified Config
import qualified Scene.Gameplay as Gameplay
import qualified Scene.Intro as Intro
import qualified Scene.Loading as Loading
import qualified Scene.Outro as Outro

main :: IO ()
main = do
  w <- initWorld
  runWith w $ do
    Loading.initialize
    playUI Config.windowDisplay black Config.windowFPS draw drawUI input tick

draw :: SystemW Picture
draw = get global >>= \case
  Loading ->
    Loading.draw
  Intro ->
    Intro.draw
  Gameplay ->
    Gameplay.draw
  Outro ->
    Outro.draw

drawUI :: Screen -> SystemW Picture
drawUI screen = do
  ui <- get global >>= \case
    Loading ->
      Loading.drawUI screen
    Intro ->
      Intro.drawUI screen
    Gameplay ->
      Gameplay.drawUI screen
    Outro ->
      Outro.drawUI screen

  fades <- Fade.drawUI screen
  pure $ ui <> fades

input :: Event -> SystemW ()
input = \case
  EventResize (newW, newH) ->
    -- Register new window size
    global $= Screen newW newH

  event ->
    get global >>= \case
      Loading ->
        Loading.input event
      Intro ->
        Intro.input event
      Gameplay ->
        Gameplay.input event
      Outro ->
        Outro.input event

tick :: Float -> SystemW ()
tick dt = do
  Delayed.tick dt
  Fade.tick dt

  get global >>= \case
    Loading ->
      Loading.tick dt
    Intro ->
      Intro.tick dt
    Gameplay ->
      Gameplay.tick dt
    Outro ->
      Outro.tick dt
