{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}

module Pact.Core.Imports
( Import(..)
)
where

import Control.DeepSeq
import Data.Text(Text)
import GHC.Generics
import Pact.Core.Names
import Pact.Core.Hash
import Pact.Core.Pretty

data Import
  = Import
  { _impModuleName  :: ModuleName
  , _impModuleHash :: Maybe ModuleHash
  , _impImported :: Maybe [Text]
  } deriving (Show, Eq, Generic)

instance NFData Import

instance Pretty Import where
  pretty (Import mn mh mhs) =
    parens $ "use" <+> pretty mn
      <> maybe mempty (\m -> space <> dquotes (pretty m)) mh
      <> maybe mempty (\m -> braces (hsep (pretty <$> m))) mhs
