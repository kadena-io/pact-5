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
import Pact.Core.IR.Eval.Runtime.Types
import Pact.Core.Persistence
import Pact.Core.Environment
import Pact.Core.Gas
import Pact.Core.IR.Eval.CEK.Types

mkBuiltinFn
  :: (IsBuiltin b)
  => i
  -> b
  -> CEKEnv step b i m
  -> NativeFunction step b i m
  -> NativeFn step b i m
mkBuiltinFn i b env fn =
  NativeFn b env fn (builtinArity b) i
{-# INLINE mkBuiltinFn #-}

{-# SPECIALIZE argsError
   :: ()
   -> CoreBuiltin
   -> [CEKValue step CoreBuiltin () Eval]
   -> Eval (EvalResult step CoreBuiltin () Eval)
    #-}
argsError
  :: (MonadEval b i m)
  => i
  -> b
  -> [CEKValue step b i m]
  -> m a
argsError info b args =
  throwExecutionError info (NativeArgumentsError (builtinName b) (toArgTypeError <$> args))

toArgTypeError :: CEKValue step b i m -> ArgTypeError
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


readOnlyEnv :: CEKEnv step b i m -> CEKEnv step b i m
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
             , _pdbTxIds = \_ _ -> dbOpDisallowed
             , _pdbGetTxLog = \_ _ -> dbOpDisallowed
             }
      in set cePactDb newPactdb e

sysOnlyEnv :: forall step b i m. CEKEnv step b i m -> CEKEnv step b i m
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
         , _pdbTxIds = \_ _ -> dbOpDisallowed
         , _pdbGetTxLog = \_ _ -> dbOpDisallowed
         }
  in set cePactDb newPactdb e
  where
  pdb = view cePactDb e
  read' :: Domain k v b i -> k -> IO (Maybe v)
  read' dom k = case dom of
    DUserTables _ -> dbOpDisallowed
    _ -> _pdbRead pdb dom k


envFromPurity :: Purity -> CEKEnv step b i m -> CEKEnv step b i m
envFromPurity PImpure = id
envFromPurity PReadOnly = readOnlyEnv
envFromPurity PSysOnly = sysOnlyEnv

enforcePactValue :: (MonadEval b i m) => i -> CEKValue step b i m -> m PactValue
enforcePactValue info = \case
  VPactValue pv -> pure pv
  _ -> throwExecutionError info ExpectedPactValue
