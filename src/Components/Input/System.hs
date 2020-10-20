module Components.Input.System
  ( attach
  , attach_
  , destroy
  , destroyAll

  , handle
  ) where

import Apecs (Entity, EntityCounter, Has, Not(..), SystemT, ($=))
import Apecs.Gloss (Event(..), Key(..), KeyState(..), Modifiers(..), Point)
import Control.Monad (guard, void, when)
import Graphics.Gloss.Data.Point (pointInBox)

import qualified Apecs
import qualified Data.List as List

import Components.Input.Types

-- * Component lifecycle

attach_
  :: ( Enum tag
     , Has world IO InputControls
     , Has world IO EntityCounter
     )
  => Entity -> tag -> SystemT world IO ()
attach_ entity tag = attach entity 0 Nothing tag

attach
  :: ( Enum tag
     , Has world IO InputControls
     , Has world IO EntityCounter
     )
  => Entity
  -> Int
  -> Maybe (Point, Point)
  -> tag
  -> SystemT world IO ()
attach entity z box tag =
  void $ Apecs.newEntity InputControls
    { _inputControlsActive = True
    , _inputControlsZ      = z
    , _inputControlsBox    = box
    , _inputControlsSchema = SchemaTag (fromEnum tag)
    , _inputControlsEntity = entity
    }

destroy :: Has world IO InputControls => Entity -> SystemT world IO ()
destroy host = Apecs.cmapM_ $ \(InputControls{..}, controls) ->
  when (_inputControlsEntity == host) $
    controls $= Not @InputControls

destroyAll :: Has world IO InputControls => SystemT world IO ()
destroyAll = Apecs.cmap $ \InputControls{..} ->
  Not @InputControls

-- * Event handling

handle
  :: Has world IO InputControls
  => [Schema world]
  -> Event
  -> SystemT world IO ()
handle schemas = \case
  EventResize{} ->
    pure ()
  EventMotion point ->
    runHandler schemas point motionPred
  EventKey key state mods point ->
    runHandler schemas point (keyPred key state mods)

-- ** Internals

runHandler
  :: Has world IO InputControls
  => [Schema world]
  -> Point
  -> (KeyTag -> [KeyTag])
  -> SystemT world IO ()
runHandler schemas point bindingPred = do
  matching <- findControls point
  case runActions schemas point bindingPred matching of
    [] ->
      pure ()
    (fusedHandler, _control) : _rest ->
      fusedHandler

findControls
  :: Has world IO InputControls
  => Point
  -> SystemT world IO [(SchemaTag, Entity)]
findControls point = fmap collect $ Apecs.cfold match []
  where
    sortKey = negate . _inputControlsZ

    collect matched = do
      InputControls{..} <- List.sortOn sortKey matched
      pure (_inputControlsSchema, _inputControlsEntity)

    match acc ic =
      if _inputControlsActive ic then
        case _inputControlsBox ic of
          Nothing ->
            add
          Just (a, b) ->
            if pointInBox point a b then
              add
            else
              skip
      else
        skip
      where
        add = ic : acc
        skip = acc

runActions
  :: [Schema world]
  -> (Float, Float)
  -> (KeyTag -> [KeyTag])
  -> [(SchemaTag, Entity)]
  -> [(SystemT world IO (), Entity)]
runActions schemas point bindingPred matching = do
  (controlTag, entity) <- matching
  Schema{..} <- schemas
  guard $ controlTag == _schemaTag
  Binding{..} <- _schemaBindings
  matched <- bindingPred _bindingKey
  pure
    ( _schemaAction entity (_bindingAction matched point)
    , entity
    )

keyPred :: Key -> KeyState -> Modifiers -> KeyTag -> [KeyTag]
keyPred key state mods kt@KeyTag{..} = do
  guard $ and
    [ kt    /= noKey
    , key   ~= _keyTagKey
    , state ~= _keyTagState
    , mods  ~= _keyTagMods
    ]
  pure pressed
  where
    a ~= mb = maybe True ((==) a) mb

    pressed = KeyTag
      { _keyTagKey   = Just key
      , _keyTagState = Just state
      , _keyTagMods  = Just mods
      }

motionPred :: KeyTag -> [KeyTag]
motionPred boundKey = do
  guard (boundKey == noKey)
  pure noKey
