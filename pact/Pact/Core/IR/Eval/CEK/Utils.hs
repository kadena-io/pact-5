{-# LANGUAGE GADTs #-}

module Pact.Core.IR.Eval.CEK.Utils
 ( mkBuiltinFn
 , argsError
 , toArgTypeError
 , sysOnlyEnv
 , readOnlyEnv
 , envFromPurity
 , enforcePactValue
 , tryNodeGas
 , unconsWorkNodeGas
 , constantWorkNodeGas) where

import Control.Lens

import Pact.Core.PactValue
import Pact.Core.Builtin
import Pact.Core.Type
import Pact.Core.Errors
import Pact.Core.Persistence
import Pact.Core.Environment
import Pact.Core.Gas
import Pact.Core.IR.Eval.CEK.Types

mkBuiltinFn
  :: (IsBuiltin b)
  => i
  -> b
  -> CEKEnv e step b i
  -> NativeFunction e step b i
  -> NativeFn e step b i
mkBuiltinFn i b env fn =
  NativeFn b env fn (builtinArity b) i
{-# INLINE mkBuiltinFn #-}

argsError
  :: IsBuiltin b
  => i
  -> b
  -> [CEKValue e step b i]
  -> EvalM e b i a
argsError info b args =
  throwExecutionError info (NativeArgumentsError (builtinName b) (toArgTypeError <$> args))

toArgTypeError :: CEKValue e step b i -> ArgTypeError
toArgTypeError = \case
  VPactValue pv -> case pv of
    PLiteral l -> ATEPrim (literalPrim l)
    PTime _ -> ATEPrim PrimTime
    PList _ -> ATEList
    PObject _ -> ATEObject
    PGuard _ -> ATEPrim PrimGuard
    PModRef _ -> ATEModRef
    PCapToken _ -> ATEClosure
  VTable{} -> ATETable
  VClosure{} -> ATEClosure

--------------------------
-- Gas-related code
--------------------------
constantWorkNodeGas :: MilliGas
constantWorkNodeGas = (MilliGas 50)

unconsWorkNodeGas :: MilliGas
unconsWorkNodeGas = (MilliGas 100)

tryNodeGas :: MilliGas
tryNodeGas = (MilliGas 100)


readOnlyEnv :: CEKEnv e step b i -> CEKEnv e step b i
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

sysOnlyEnv :: forall e step b i. CEKEnv e step b i -> CEKEnv e step b i
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
  read' :: Domain k v b i -> k -> IO (Maybe v)
  read' dom k = case dom of
    DUserTables _ -> dbOpDisallowed
    _ -> _pdbRead pdb dom k


envFromPurity :: Purity -> CEKEnv e step b i -> CEKEnv e step b i
envFromPurity PImpure = id
envFromPurity PReadOnly = readOnlyEnv
envFromPurity PSysOnly = sysOnlyEnv

enforcePactValue :: i -> CEKValue e step b i -> EvalM e b i PactValue
enforcePactValue info = \case
  VPactValue pv -> pure pv
  _ -> throwExecutionError info ExpectedPactValue
