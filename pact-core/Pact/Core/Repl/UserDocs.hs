module Pact.Core.Repl.UserDocs where

import Control.Lens
import qualified Data.Map.Strict as M
import Pact.Core.Builtin
import Pact.Core.Repl.Utils
import Pact.Core.Names

import qualified Pact.Core.Syntax.ParseTree as Lisp

functionDocs
  :: ModuleName
  -- ^ the mangled module name
  -> Lisp.TopLevel i
  -- The original module syntax
  -> ReplM ReplCoreBuiltin ()
functionDocs mn = \case
  Lisp.TLModule md ->
    () <$ traverse addModuleDoc (Lisp._mDefs md)
  _ -> pure ()
  where
  addModuleDoc d = do
    let qualName = QualifiedName (Lisp.defName d) mn
    traverse (\docs -> replUserDocs  %= M.insert qualName docs) (Lisp.defDocs d)
