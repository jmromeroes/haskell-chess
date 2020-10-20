module Components.Input.Schema
  ( schema
  , Bindings

  , BindKey
  , keyDown
  , keyUp
  , keyToggle

  , BindPoint
  , mouseMove
  , mouseDown
  , mouseUp
  , mouseToggle

  -- * Gloss re-exports
  , Point
  , Event
  , Key(..)
  , KeyState(..)
  , SpecialKey(..)
  , MouseButton(..)

  , module Components.Input.Modifiers
  ) where

import Apecs
import Apecs.Gloss (Event(..), Key(..), KeyState(..), SpecialKey(..), MouseButton(..), Point)

import Components.Input.Types
import Components.Input.Modifiers (Modifiers, modAlt, modCtrl, modShift, (+>))

schema
  :: Enum tag
  => tag
  -> (Entity -> action -> SystemT world IO ())
  -> Bindings action
  -> Schema world
schema tag actions bindings = Schema
  { _schemaTag      = SchemaTag (fromEnum tag)
  , _schemaAction   = actions
  , _schemaBindings = mconcat bindings
  }

type Bindings action = [[Binding action]]

-- ** Keyboard controls

type BindKey key action = key -> action -> [Binding action]

keyDown :: ToKeyTag key => BindKey key action
keyDown key action = pure Binding
  { _bindingKey    = toKeyTag key
  , _bindingAction = \_kt _pos -> action
  }

keyUp :: ToKeyTag key => BindKey key action
keyUp key action = pure Binding
  { _bindingKey    = toKeyTagUp key
  , _bindingAction = \_kt _pos -> action
  }

keyToggle
  :: ToKeyTag key
  => key -> action -> action -> [Binding action]
keyToggle key down up =
  keyDown key down <>
  keyUp key up

-- ** Mouse controls (i.e. having position)

type BindPoint action = (Point -> action) -> [Binding action]

mouseMove :: BindPoint action
mouseMove pointAction = pure Binding
  { _bindingKey    = noKey
  , _bindingAction = const pointAction
  }

mouseDown :: ToKeyTag key => key -> BindPoint action
mouseDown button pointAction = pure Binding
  { _bindingKey    = toKeyTag button
  , _bindingAction = const pointAction
  }

mouseUp :: ToKeyTag key => key -> BindPoint action
mouseUp button pointAction = pure Binding
  { _bindingKey    = toKeyTagUp button
  , _bindingAction = const pointAction
  }

mouseToggle
  :: ToKeyTag key
  => key
  -> (Point -> action)
  -> (Point -> action)
  -> [Binding action]
mouseToggle button down up =
  mouseDown button down <>
  mouseUp button up
