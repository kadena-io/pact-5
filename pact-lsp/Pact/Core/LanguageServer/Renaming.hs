-- |

module Pact.Core.LanguageServer.Renaming where

import Control.Lens
import Pact.Core.Builtin
import Pact.Core.IR.Term
import Pact.Core.Names
import Pact.Core.Info

import Pact.Core.LanguageServer.Utils

import Data.Text (Text)
import Data.List
import Data.Maybe


matchingDefs
  :: [EvalTopLevel ReplCoreBuiltin i]
  -> ModuleName
  -> Text
  -> (Maybe (EvalIfDef ReplCoreBuiltin i), Maybe (EvalDef ReplCoreBuiltin i))
matchingDefs tls mn n = (interfaceDef, moduleDef)
  where
    interfaceDef = do
      let p = \case
            TLInterface (Interface mn' _ _ _ _ _ _) -> mn == mn'
            _ -> False

      TLInterface interf <- find p tls
      find (\x -> ifDefName x == n) (_ifDefns interf)

    moduleDef = do
      let p = \case
            TLModule (Module mn' _ _ _ _ _ _ _ _ _) -> mn == mn'
            _ -> False

      TLModule module' <- find p tls
      find (\x -> defName x == n) (_mDefs module')


matchingTerms
  :: forall i. ()
  => (EvalTerm ReplCoreBuiltin i -> Bool)
  -> EvalTopLevel ReplCoreBuiltin i
  -> [EvalTerm ReplCoreBuiltin i]
matchingTerms predicate topLevel = let
  terms = toListOf topLevelTerms topLevel
  in concatMap (toListOf filteredTerms) terms
  where
  filteredTerms :: Traversal'
    (EvalTerm ReplCoreBuiltin i) (EvalTerm ReplCoreBuiltin i)
  filteredTerms = traverseTerm . filtered predicate




getRenameSpanInfo
  :: HasSpanInfo i
  => [EvalTopLevel ReplCoreBuiltin i]
  -> PositionMatch ReplCoreBuiltin i
  -> [SpanInfo]
getRenameSpanInfo tls = \case
   TermMatch (Var (Name n vt) _) -> case vt of
     NBound _db -> mempty
     NTopLevel mn _mh -> do
       let isSameVar = \case
             Var (Name n' vt') _ ->  n == n' && vt == vt'
             _ -> False
           termOccurences = toListOf (each . termInfo) $ concatMap (matchingTerms isSameVar) tls
           (mInterfPos, mDefPos) = bimap (fmap ifDefNameInfo) (fmap defNameInfo) (matchingDefs tls mn n)
       fmap (view spanInfo) $ concat [maybeToList mInterfPos, maybeToList mDefPos, termOccurences]
     _ -> mempty
   DefunMatch (Defun spec _args _body _) -> do
     let dName = _argName spec
         isSameVar = \case
           Var (Name n _) _ -> n == dName
           _ -> False
         termOccurences = toListOf (each . termInfo . spanInfo) $ concatMap (matchingTerms isSameVar) tls
     view spanInfo (_argInfo spec) : termOccurences
   _ -> mempty
