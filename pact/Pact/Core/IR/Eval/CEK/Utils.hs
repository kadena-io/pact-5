{-# LANGUAGE GADTs #-}

module Pact.Core.IR.Eval.CEK.Utils
 ( mkBuiltinFn
 , argsError
 , toArgTypeError
 , sysOnlyEnv
 , readOnlyEnv
 , envFromPurity
 , enforcePactValue
 , enforceSaturatedApp
 ) where

import Control.Lens

import Pact.Core.PactValue
import Pact.Core.Builtin
import Pact.Core.Type
import Pact.Core.Errors
import Pact.Core.Persistence
import Pact.Core.Environment
import Pact.Core.IR.Eval.CEK.Types

mkBuiltinFn
  :: (IsBuiltin b)
  => i
  -> b
  -> CEKEnv e b i
  -> NativeFunction e b i
  -> NativeFn e b i
mkBuiltinFn i b env fn =
  NativeFn b env fn (builtinArity b) i
{-# INLINE mkBuiltinFn #-}

argsError
  :: IsBuiltin b
  => i
  -> b
  -> [CEKValue e b i]
  -> EvalM e b i a
argsError info b args =
  throwExecutionError info (NativeArgumentsError (builtinName b) (toArgTypeError <$> args))

toArgTypeError :: CEKValue e b i -> ArgTypeError
toArgTypeError = \case
  VPactValue pv -> case pv of
    PLiteral l -> ATEPrim (literalPrim l)
    PTime _ -> ATEPrim PrimTime
    PList _ -> ATEList
    PObject _ -> ATEObject
    PGuard _ -> ATEPrim PrimGuard
    PModRef _ -> ATEModRef
    PCapToken _ -> ATEClosure
    PTable _ -> ATETable
  VClosure{} -> ATEClosure

--------------------------
-- Gas-related code
--------------------------


readOnlyEnv :: CEKEnv e b i -> CEKEnv e b i
readOnlyEnv e
  | view (cePactDb . pdbPurity) e == PSysOnly = e
  | otherwise =
      let pdb = view cePactDb e
          newPactdb =
              PactDb
             { _pdbPurity = PReadOnly
             , _pdbRead = _pdbRead pdb
             , _pdbWrite = \_ _ _ _ -> dbOpDisallowed
             , _pdbKeys = \_ -> dbOpDisallowed
             , _pdbCreateUserTable = \_ -> dbOpDisallowed
             , _pdbBeginTx = \_ -> dbOpDisallowed
             , _pdbCommitTx = dbOpDisallowed
             , _pdbRollbackTx = dbOpDisallowed
             }
      in set cePactDb newPactdb e

sysOnlyEnv :: forall e b i. CEKEnv e b i -> CEKEnv e b i
sysOnlyEnv e
  | view (cePactDb . pdbPurity) e == PSysOnly = e
  | otherwise =
  let newPactdb =
          PactDb
         { _pdbPurity = PSysOnly
         , _pdbRead = read'
         , _pdbWrite = \_ _ _ _ -> dbOpDisallowed
         , _pdbKeys = const dbOpDisallowed
         , _pdbCreateUserTable = \_ -> dbOpDisallowed
         , _pdbBeginTx = const dbOpDisallowed
         , _pdbCommitTx = dbOpDisallowed
         , _pdbRollbackTx = dbOpDisallowed
         }
  in set cePactDb newPactdb e
  where
  pdb = view cePactDb e
  read' :: Domain k v b i -> k -> GasM b i (Maybe v)
  read' dom k = case dom of
    DUserTables _ -> dbOpDisallowed
    _ -> _pdbRead pdb dom k


envFromPurity :: Purity -> CEKEnv e b i -> CEKEnv e b i
envFromPurity PImpure = id
envFromPurity PReadOnly = readOnlyEnv
envFromPurity PSysOnly = sysOnlyEnv

enforcePactValue :: i -> CEKValue e b i -> EvalM e b i PactValue
enforcePactValue info = \case
  VPactValue pv -> pure pv
  _ -> throwExecutionError info ExpectedPactValue

invalidArgs
  :: i
  -> ErrorClosureType
  -> Int
  -> Int
  -> EvalM e b i a
invalidArgs info mn expected actual =
  throwExecutionError info $ InvalidNumArgs mn expected actual

enforceSaturatedApp :: IsBuiltin b => i -> CEKValue e b i -> EvalM e b i ()
enforceSaturatedApp info = \case
  VPactValue _ -> pure ()
  VClosure clo -> case clo of
    PC pc ->
      invalidArgs info (maybe ErrClosureLambda ErrClosureUserFun (_sfName <$> _pcloFrame pc)) (_pcloArity pc + _pcloNArgs pc) (_pcloNArgs pc)
    PN pn ->
      let nargs = length (_pNativeAppliedArgs pn)
      in invalidArgs info (ErrClosureNativeFun (builtinName (_pNative pn))) (_pNativeArity pn + nargs) nargs
    _ -> pure ()
{-# INLINE enforceSaturatedApp #-}
