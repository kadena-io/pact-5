module Pact.Core.IR.ConstEval
 ( evalModuleDefConsts
 , evalIfaceDefConsts
 , evalTLConsts ) where

import Control.Lens
import qualified Data.Map.Strict as M

import Pact.Core.Names
import Pact.Core.Type
import Pact.Core.Environment
import Pact.Core.Persistence
import Pact.Core.IR.Term
import Pact.Core.IR.Eval.Runtime ( maybeTCType )
import Pact.Core.Interpreter

evalTLConsts
  :: Interpreter e b i
  -> TopLevel Name Type b i
  -> EvalM e b i (TopLevel Name Type b i)
evalTLConsts bEnv = \case
  TLTerm t -> pure $ TLTerm t
  TLInterface ti -> TLInterface <$> evalIfaceDefConsts bEnv ti
  TLModule m -> TLModule <$> evalModuleDefConsts bEnv m
  TLUse u i -> pure $ TLUse u i

-- Todo: this may need a different IR for module, or at least a newtype wrapper over `Name`
evalModuleDefConsts
  :: Interpreter e b i
  -> Module Name Type b i
  -> EvalM e b i (Module Name Type b i)
evalModuleDefConsts interpreter (Module mname mgov defs blessed imports implements mhash txh info) = do
  lo <- use esLoaded
  defs' <- traverse go defs
  esLoaded .= lo
  pure (Module mname mgov defs' blessed imports implements mhash txh info)
  where
  go defn = do
    d' <- case defn of
      DConst dc -> case _dcTerm dc of
        TermConst term -> do
          pv <- eval interpreter PSysOnly term
          maybeTCType (_dcInfo dc) (_argType $ _dcSpec dc) pv
          pure (DConst (set dcTerm (EvaledConst pv) dc))
        EvaledConst _ -> pure defn
      _ -> pure defn
    let dn = defName defn
    let fqn = FullyQualifiedName mname dn mhash
    loAllLoaded %= M.insert fqn d'
    pure d'


-- Todo: this may need a different IR for module, or at least a newtype wrapper over `Name`
evalIfaceDefConsts
  :: Interpreter e b i
  -> Interface Name Type b i
  -> EvalM e b i (Interface Name Type b i)
evalIfaceDefConsts interpreter (Interface ifname ifdefns imps ifh txh info) = do
  lo <- use esLoaded
  ifdefns' <- traverse go ifdefns
  esLoaded .= lo
  pure (Interface ifname ifdefns' imps ifh txh info)
  where
  go defn = case defn of
      IfDConst dc -> case _dcTerm dc of
        TermConst term -> do
          pv <- eval interpreter PSysOnly term
          let dn = _argName $ _dcSpec dc
              fqn = FullyQualifiedName ifname dn ifh
          loAllLoaded %= M.insert fqn (DConst dc)
          pure (IfDConst (set dcTerm (EvaledConst pv) dc))
        EvaledConst _ -> pure defn
      _ -> pure defn
