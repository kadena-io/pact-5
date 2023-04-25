{-# LANGUAGE DeriveTraversable #-}


module Pact.Core.Capabilities
 ( DefCapMeta(..)
 , CapForm(..)
 , capFormName
 ) where

import Control.Lens
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
  deriving (Show, Functor, Foldable, Traversable)

capFormName :: Lens (CapForm name e) (CapForm name' e) name name'
capFormName f = \case
  WithCapability name es e -> (\fq -> WithCapability fq es e) <$> f name
  RequireCapability name es -> (`RequireCapability` es) <$> f name
  ComposeCapability name es -> (`ComposeCapability` es) <$> f name
  InstallCapability name es -> (`InstallCapability` es) <$> f name
  EmitEvent name es -> (`EmitEvent` es) <$> f name
