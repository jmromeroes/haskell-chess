{-# LANGUAGE TemplateHaskell #-}

module Components.Input.Modifiers
  ( HasMods(..)
  , setMod
  , (+>)

  , modAlt
  , modCtrl
  , modShift

  , Modifiers(..)
  , modsUp
  ) where

import Control.Lens
import Control.Lens.TH (makeLensesFor)
import Graphics.Gloss.Interface.IO.Interact

import Components.Input.Types

class HasMods a where
  modsL :: Lens' a (Maybe Modifiers)

instance HasMods KeyTag where
  modsL = keyTagMods

setMod :: HasMods t => Lens' Modifiers KeyState -> t -> t
setMod modLens = modsL . non modsUp . modLens .~ Down

infixr 4 +>
(+>)
  :: ToKeyTag a
  => Lens' Modifiers KeyState
  -> a
  -> KeyTag
modLens +> x = setMod modLens (toKeyTag x)

makeLensesFor
  [ ("alt",   "modAlt")
  , ("ctrl",  "modCtrl")
  , ("shift", "modShift")
  ]
  ''Modifiers
