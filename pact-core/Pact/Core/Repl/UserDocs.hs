module Pact.Core.Repl.UserDocs where

import Control.Lens
import qualified Data.Map.Strict as M
import Pact.Core.Builtin
import Pact.Core.Repl.Utils
import Pact.Core.Names
import Pact.Core.Info
import Pact.Core.Environment
import qualified Pact.Core.Syntax.ParseTree as Lisp

functionDocs
  :: Lisp.TopLevel SpanInfo
  -- The original module syntax
  -> ReplM ReplCoreBuiltin ()
functionDocs = \case
  Lisp.TLModule md -> do
    mn <- mangleNamespace (Lisp._mName md)
    () <$ traverse (addModuleDoc mn) (Lisp._mDefs md)
  _ -> pure ()
  where
  addModuleDoc mn d = do
    let qualName = QualifiedName (Lisp.defName d) mn
    traverse (\docs -> replUserDocs  %= M.insert qualName (docs, Lisp.defInfo d)) (Lisp.defDocs d)
