{-# LANGUAGE TemplateHaskell #-}

module Components.Input.Types where

import Apecs
import Control.Lens.TH (makeLenses)
import Apecs.Gloss
import Graphics.Gloss.Data.Point (Point)

data InputControls = InputControls
  { _inputControlsActive :: Bool
  , _inputControlsZ      :: Int
  , _inputControlsBox    :: Maybe (Point, Point)
  , _inputControlsSchema :: SchemaTag
  , _inputControlsEntity :: Entity
  }
  deriving (Eq, Ord, Show)

instance Component InputControls where
  type Storage InputControls = Map InputControls

data KeyTag = KeyTag
  { _keyTagKey   :: Maybe Key
  , _keyTagState :: Maybe KeyState
  , _keyTagMods  :: Maybe Modifiers
  }
  deriving (Eq, Ord, Show)

noKey :: KeyTag
noKey = KeyTag
  { _keyTagKey   = Nothing
  , _keyTagState = Nothing
  , _keyTagMods  = Nothing
  }

modsUp :: Modifiers
modsUp = Modifiers
  { alt   = Up
  , ctrl  = Up
  , shift = Up
  }

newtype SchemaTag = SchemaTag { unSchemaTag :: Int }
  deriving (Eq, Ord, Show)

data Schema world = forall action . Schema
  { _schemaTag      :: SchemaTag
  , _schemaAction   :: Entity -> action -> SystemT world IO ()
  , _schemaBindings :: [Binding action]
  }

instance Show (Schema world) where
  show Schema{..} = unwords
    [ "Schema {"
    , "tag =", show _schemaTag
    , "bindings =", show _schemaBindings
    , "}"
    ]

data Binding action = Binding
  { _bindingKey    :: KeyTag
  , _bindingAction :: KeyTag -> Point -> action
  }

instance Show (Binding action) where
  show Binding{..} = unwords
    [ "Binding {"
    , "key =", show _bindingKey
    , "}"
    ]

class ToKeyTag a where
  toKeyTag :: a -> KeyTag

  toKeyTagUp :: a -> KeyTag
  toKeyTagUp a =
    (toKeyTag a) { _keyTagState = Just Up }

instance ToKeyTag KeyTag where
  toKeyTag = id

instance ToKeyTag Key where
  toKeyTag = \case
    Char c        -> toKeyTag c
    SpecialKey sk -> toKeyTag sk
    MouseButton b -> toKeyTag b

instance ToKeyTag Char where
  toKeyTag c = KeyTag
    { _keyTagKey   = Just (Char c)
    , _keyTagState = Just Down
    , _keyTagMods  = Just modsUp
    }

instance ToKeyTag SpecialKey where
  toKeyTag sk = KeyTag
    { _keyTagKey   = Just (SpecialKey sk)
    , _keyTagState = Just Down
    , _keyTagMods  = Just modsUp
    }

instance ToKeyTag MouseButton where
  toKeyTag btn = KeyTag
    { _keyTagKey   = Just (MouseButton btn)
    , _keyTagState = Just Down
    , _keyTagMods  = Just modsUp
    }

makeLenses ''InputControls
makeLenses ''KeyTag
