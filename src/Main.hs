{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import Apecs

import SDL.Vect
import SDL(($=))
import qualified SDL
import qualified SDL.Image(quit)
import qualified SDL.Font

import Control.Monad
import System.Exit (exitSuccess)

import Types
import Common
import Components
import EventHandler
import Draw
import WorldSimulation
import Resources
import GameMap
import Characters

-- Initialises the world with it's first system:
-- this system simply creates an entity
initialise :: SDL.WindowConfig -> SDL.Renderer -> [TexResource] -> [FontResource] -> System' ()
initialise conf r t f = void $ do
  let ws = fromIntegral <$> SDL.windowInitialSize conf
  set global $ WindowSize ws
  set global $ Renderer $ Just r
  set global $ Textures $ createResourceMap t
  set global $ Fonts $ createResourceMap f
  set global $ GameMap $ generateIdentityMap (V2 40 32)
  void $ newEntity
    ( Queen White
    , Position (V2 10 10)
    , CellRef 1 2
    , Sprite "Assets/WQueen.png" (SDL.Rectangle (P (V2 0 0)) (V2 764 696)))
  void $ newEntity
    ( King White
    , Position (V2 10 10)
    , CellRef 1 1
    , Sprite "Assets/WKing.png" (SDL.Rectangle (P (V2 0 0)) (V2 771 777)))
  void $ newEntity
    ( Rook White
    , Position (V2 10 10)
    , CellRef 1 3
    , Sprite "Assets/WRook.png" (SDL.Rectangle (P (V2 0 0)) (V2 476 585)))
  void $ newEntity
    ( Bishop White 
    , Position (V2 10 10)
    , CellRef playerCellRef
    , Sprite "Assets/WBishop.png" (SDL.Rectangle (P (V2 0 0)) (V2 721 728)))
  void $ newEntity
    ( Knight White
    , Position (V2 10 10)
    , CellRef playerCellRef
    , Sprite "Assets/WKnight.png" (SDL.Rectangle (P (V2 0 0)) (V2 620 695)))
  void $ newEntity
    ( Pawn White
    , Position (V2 10 10)
    , CellRef playerCellRef
    , Sprite "Assets/WPawn.png" (SDL.Rectangle (P (V2 0 0)) (V2 476 585)))
  void $ newEntity
    ( Queen Black
    , Position (V2 10 10)
    , CellRef 1 2
    , Sprite "Assets/BQueen.png" (SDL.Rectangle (P (V2 0 0)) (V2 764 696)))
  void $ newEntity
    ( King Black
    , Position (V2 10 10)
    , CellRef 1 1
    , Sprite "Assets/BKing.png" (SDL.Rectangle (P (V2 0 0)) (V2 771 777)))
  void $ newEntity
    ( Rook Black
    , Position (V2 10 10)
    , CellRef 1 3
    , Sprite "Assets/BRook.png" (SDL.Rectangle (P (V2 0 0)) (V2 476 585)))
  void $ newEntity
    ( Bishop Black 
    , Position (V2 10 10)
    , CellRef playerCellRef
    , Sprite "Assets/BBishop.png" (SDL.Rectangle (P (V2 0 0)) (V2 721 728)))
  void $ newEntity
    ( Knight Black
    , Position (V2 10 10)
    , CellRef playerCellRef
    , Sprite "Assets/BKnight.png" (SDL.Rectangle (P (V2 0 0)) (V2 620 695)))
  void $ newEntity
    ( Pawn Black
    , Position (V2 10 10)
    , CellRef playerCellRef
    , Sprite "Assets/BPawn.png" (SDL.Rectangle (P (V2 0 0)) (V2 476 585)))

-- When called, manipulates the global time component
incrTime :: Double -> System' ()
incrTime dT = modify 0 $ \(Time t) -> Time (t+dT)

-- Runs a system periodically
triggerEvery :: Double -> Double -> Double -> System' a -> System' ()
triggerEvery dT period phase sys = do
  Time t <- get global
  let t' = t + phase
      trigger = floor (t'/period) /= floor ((t'+dT)/period)
  when trigger $ void sys

-- Main step system
step :: Double -> System' ()
step dT = do
  triggerEvery dT 50 0 $ do
    PlayerReady ready <- get global
    when ready $ do
      [(Player, CellRef pos)] <- getAll
      executePlayerPath pos
  incrTime dT
  floatTooltips dT
  snapEntities

-- Main program thread
main :: IO ()
main = do
  world <- initWorld

  -- Initialise SDL
  SDL.initialize [SDL.InitVideo]
  SDL.Font.initialize

  -- Create a window and renderer
  let windowConfig = 
        SDL.defaultWindow
          { SDL.windowInitialSize = V2 1920 1080
          , SDL.windowMode = SDL.Windowed
          , SDL.windowHighDPI = True
          }
  window <- SDL.createWindow "App" windowConfig
  renderer <-
      SDL.createRenderer window (-1)
        SDL.RendererConfig
          { SDL.rendererType = SDL.AcceleratedRenderer
          , SDL.rendererTargetTexture = False
          }

  -- Load resources and initialise game
  texs <- loadTextures renderer ["Assets/BBishop.png", "Assets/BKing.png", "Assets/BKinght.png", "Assets/BPawn.png", "Assets/BQueen.png", "Assets/BRook.png", "Assets/WBishop.png", "Assets/WKing.png", "Assets/WKinght.png", "Assets/WPawn.png", "Assets/WQueen.png", "Assets/WRook.png"]
  fonts <- loadFonts [("Assets/Roboto-Regular.ttf", 12)]
  runSystem (initialise windowConfig renderer texs fonts) world

  -- Set up logical world size
  let ws = let V2 tsw tsh = tileSize in V2 (8 * tsw) (8 * tsh)
  SDL.rendererLogicalSize renderer $= Just (fromIntegral <$> ws)

  -- Display the game
  SDL.showWindow window

  let loop prevTicks secondTick fpsAcc prevFps = do
        ticks <- SDL.ticks
        payload <- map SDL.eventPayload <$> SDL.pollEvents
        let quit = SDL.QuitEvent `elem` payload
            dt = ticks - prevTicks
            calcFps = secondTick + dt > 1000
            newFps = if calcFps then fpsAcc + 1 else prevFps
            newFpsAcc = if calcFps then 1 else fpsAcc + 1
            newSecondTick = if calcFps then mod (secondTick + dt) 1000 else secondTick + dt

        runSystem (handlePayload payload) world
        runSystem (step $ fromIntegral dt) world

        SDL.rendererRenderTarget renderer $= Nothing
        SDL.rendererDrawColor renderer $= V4 0 0 0 0
        SDL.clear renderer

        runSystem (draw renderer newFps worldsize) world
        runSystem clearMessages world

        SDL.present renderer
        unless quit $ loop ticks newSecondTick newFpsAcc newFps

  -- Begin looping
  loop 0 0 0 0

  -- Clean up
  runSystem releaseData world
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.Image.quit
  SDL.Font.quit
  SDL.quit
  putStrLn "Goodbye! (◠‿◠✿)"
  exitSuccess

-- Release textures
releaseData :: System' ()
releaseData = do
  Textures t <- get global
  mapM_ SDL.destroyTexture t
  Fonts f <- get global
  mapM_ SDL.Font.free f
