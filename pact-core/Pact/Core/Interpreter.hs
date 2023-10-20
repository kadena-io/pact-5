{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Pact.Core.Interpreter
  ( Interpreter(..)
  , InterpretValue(..)
  )where

import Pact.Core.Type
import Pact.Core.IR.Term
import Pact.Core.Info
import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.PactValue


-- | Our general interpreter abstraction. It allows us to
-- decouple evaluation from
data Interpreter b i m
  = Interpreter
  { _interpret :: !(Term Name Type b i -> m InterpretValue)
  , _interpretGuard :: !(i -> Guard FullyQualifiedName PactValue -> m InterpretValue)
  }

data InterpretValue
  = IPV PactValue SpanInfo
  | IPClosure
  | IPTable TableName
  deriving Show

-- interpretGuardEnforce
--   :: (MonadEval b i m)
--   => i
--   -> Interpreter b i m
--   -> Guard FullyQualifiedName PactValue
--   -> m Bool
-- interpretGuardEnforce info interp = \case
--   GKeyset ks ->  enforceKeyset ks
--   GKeySetRef ksn -> do
--     epdb <- viewEvalEnv eePactDb
--     enforceKeysetName info epdb ksn
--   GUserGuard ug -> runUserGuard info cont handler env ug
--   GCapabilityGuard cg -> enforceCapGuard cg
--   GModuleGuard (ModuleGuard mn _) -> calledByModule mn >>= \case
--     True -> pure True
--     False -> do
--       epdb <- viewEvalEnv eePactDb
--       cond <- interpretModuleAdmin epdb mn
--       if cond then (esCaps . csModuleAdmin %== S.insert mn) *> pure True
--       else pure cond
--   where
--   enforceCapGuard (CapabilityGuard fqn args mpid) = do
--     case mpid of
--       Nothing -> enforceCap
--       Just pid -> do
--         currPid <- getPactId info
--         if currPid == pid then enforceCap
--         else pure False
--     where
--     enforceCap = isCapInStack (CapToken fqn args)

--   interpretModuleAdmin pdb moduleName = getModule info pdb moduleName >>= \case
--     targetModule -> do
--       case _mGovernance targetModule of
--         KeyGov ksn -> enforceKeysetName info pdb ksn
--         CapGov (ResolvedGov fqn) -> do
--           let cgBody = Constant LUnit info
--               term = CapabilityForm (WithCapability (fqnToName fqn) [] cgBody) info
--           True <$ (_interpret interp term)
