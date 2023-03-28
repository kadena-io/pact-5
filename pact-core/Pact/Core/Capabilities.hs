{-# LANGUAGE DeriveTraversable #-}


module Pact.Core.Capabilities where

import Data.Text(Text)


data DefCapMeta name
  = DefEvent
  | DefManaged (Maybe (Text, name))
  deriving Show

data CapForm name e
  = WithCapability name [e] e
  | RequireCapability name [e]
  | ComposeCapability name [e]
  | InstallCapability name [e]
  | EmitEvent name [e]
  deriving (Functor, Foldable, Traversable)
