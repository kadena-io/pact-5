module Pact.Core.Repl.UserDocs where

import qualified Data.Map.Strict as M
import Pact.Core.Builtin
import Pact.Core.Repl.Utils
import Pact.Core.Names
import Pact.Core.Info
import Pact.Core.Environment
import qualified Pact.Core.Syntax.ParseTree as Lisp
import Data.Foldable (traverse_)

functionDocs
  :: Lisp.TopLevel SpanInfo
  -- The original module syntax
  -> ReplM ReplCoreBuiltin ()
functionDocs = \case
  Lisp.TLModule md -> do
    mn <- mangleNamespace (ModuleName (Lisp._mName md) Nothing)
    traverse_ (addModuleDoc mn) (Lisp._mDefs md)
  _ -> pure ()
  where
  addModuleDoc mn d = do
    let qualName = QualifiedName (Lisp.defName d) mn
    traverse_ (\docs -> replUserDocs %== M.insert qualName docs) (Lisp.defDocs d)
    replTLDefPos %== M.insert qualName (Lisp.defInfo d)
