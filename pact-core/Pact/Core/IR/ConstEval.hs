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
import Pact.Core.IR.Eval.Runtime
import qualified Pact.Core.IR.Eval.CEK as Eval

evalTLConsts :: (MonadEval b i m, Eval.CEKEval step b i m) => BuiltinEnv step b i m -> TopLevel Name Type b i -> m (TopLevel Name Type b i)
evalTLConsts bEnv = \case
  TLTerm t -> pure $ TLTerm t
  TLInterface ti -> TLInterface <$> evalIfaceDefConsts bEnv ti
  TLModule m -> TLModule <$> evalModuleDefConsts bEnv m
  TLUse u i -> pure $ TLUse u i

-- Todo: this may need a different IR for module, or at least a newtype wrapper over `Name`
evalModuleDefConsts
  :: (MonadEval b i m, Eval.CEKEval step b i m)
  => BuiltinEnv step b i m
  -> Module Name Type b i
  -> m (Module Name Type b i)
evalModuleDefConsts bEnv (Module mname mgov defs blessed imports implements mhash info) = do
  lo <- useEvalState esLoaded
  defs' <- traverse go defs
  esLoaded .== lo
  pure (Module mname mgov defs' blessed imports implements mhash info)
  where
  go defn = do
    d' <- case defn of
      DConst dc -> case _dcTerm dc of
        TermConst term -> do
          pv <- Eval.eval PSysOnly bEnv term
          pure (DConst (set dcTerm (EvaledConst pv) dc))
        EvaledConst _ -> pure defn
      _ -> pure defn
    let dn = defName defn
    let fqn = FullyQualifiedName mname dn mhash
    loAllLoaded %== M.insert fqn d'
    pure d'


-- Todo: this may need a different IR for module, or at least a newtype wrapper over `Name`
evalIfaceDefConsts
  :: (MonadEval b i m, Eval.CEKEval step b i m)
  => BuiltinEnv step b i m
  -> Interface Name Type b i
  -> m (Interface Name Type b i)
evalIfaceDefConsts bEnv (Interface ifname ifdefns imps ifh info) = do
  lo <- useEvalState esLoaded
  ifdefns' <- traverse go ifdefns
  esLoaded .== lo
  pure (Interface ifname ifdefns' imps ifh info)
  where
  go defn = case defn of
      IfDConst dc -> case _dcTerm dc of
        TermConst term -> do
          pv <- Eval.eval PSysOnly bEnv term
          let dn = _dcName dc
              fqn = FullyQualifiedName ifname dn ifh
          loAllLoaded %== M.insert fqn (DConst dc)
          pure (IfDConst (set dcTerm (EvaledConst pv) dc))
        EvaledConst _ -> pure defn
      _ -> pure defn
