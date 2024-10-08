-- |

module Pact.Core.LanguageServer.Renaming where

import Control.Lens
import Pact.Core.Builtin
import Pact.Core.IR.Term
import Pact.Core.Names
import Pact.Core.Info
import Pact.Core.Type

import Pact.Core.LanguageServer.Utils

import Data.Text (Text)
import Data.List
import Data.Maybe


matchingDefs
  :: [EvalTopLevel ReplCoreBuiltin SpanInfo]
  -> ModuleName
  -> Text
  -> (Maybe (EvalIfDef ReplCoreBuiltin SpanInfo), Maybe (EvalDef ReplCoreBuiltin SpanInfo))
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
  :: (EvalTerm ReplCoreBuiltin SpanInfo -> Bool)
  -> EvalTopLevel ReplCoreBuiltin SpanInfo
  -> [EvalTerm ReplCoreBuiltin SpanInfo]
matchingTerms predicate topLevel = let
  terms = toListOf topLevelTerms topLevel
  in concatMap (toListOf filteredTerms) terms
  where
  filteredTerms :: Traversal'
    (EvalTerm ReplCoreBuiltin SpanInfo) (EvalTerm ReplCoreBuiltin SpanInfo)
  filteredTerms = traverseTerm . filtered predicate




getRenameSpanInfo
  :: [EvalTopLevel ReplCoreBuiltin SpanInfo]
  -> PositionMatch ReplCoreBuiltin SpanInfo
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
       concat [maybeToList mInterfPos, maybeToList mDefPos, termOccurences]
     _ -> mempty
   DefunMatch (Defun spec _args _body _) -> do
     let dName = _argName spec
         isSameVar = \case
           Var (Name n _) _ -> n == dName
           _ -> False
         termOccurences = toListOf (each . termInfo) $ concatMap (matchingTerms isSameVar) tls
     _argInfo spec : termOccurences
   _ -> mempty
