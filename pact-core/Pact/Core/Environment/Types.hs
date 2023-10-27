{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE InstanceSigs #-}

module Pact.Core.Environment.Types
 ( EvalEnv(..)
 , eeMsgSigs, eePactDb
 , eeHash, eeMsgBody
 , eeDefPactStep
 , eePublicData, eeMode, eeFlags
 , eeNatives
 , eeNamespacePolicy
 , PactState(..)
 , psLoaded
 , TxCreationTime(..)
 , PublicData(..)
 , pdPublicMeta, pdBlockHeight
 , pdBlockTime, pdPrevBlockHash
 , PublicMeta(..)
 , pmChainId, pmSender, pmGasLimit
 , pmGasPrice, pmTTL, pmCreationTime
 , TTLSeconds(..)
 , ChainId(..)
 , cdChainId, cdBlockHeight
 , cdBlockTime, cdPrevBlockHash
 , cdSender, cdGasLimit, cdGasPrice
 , EvalState(..)
 , HasEvalState(..)
 , StackFrame(..)
 , StackFunctionType(..)
 , flagRep
 , flagReps
 , ExecutionFlag(..)
 , MonadEvalEnv(..)
 , MonadEvalState(..)
 , MonadEval
 , defaultEvalEnv
 ) where


import Control.Lens
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Set(Set)
import Data.Text(Text)
import Data.Map.Strict(Map)
import Data.Default

import qualified Data.Text as T
import qualified Data.Map.Strict as M

import Pact.Core.Persistence
import Pact.Core.Capabilities
import Pact.Core.Guards
import Pact.Core.PactValue
import Pact.Core.Hash
import Pact.Core.Names
import Pact.Core.DefPacts.Types
import Pact.Core.ChainData
import Pact.Core.Errors
import Pact.Core.Gas
import Pact.Core.Namespace

-- | Execution flags specify behavior of the runtime environment,
-- with an orientation towards some alteration of a default behavior.
-- Thus, a flag should _not_ describe "normal behavior" (the default),
-- but instead should enable some "unusual" option.
data ExecutionFlag
  -- | Disable user module install
  = FlagDisableModuleInstall
  -- | Disable database history queries in transactional mode (local-only)
  | FlagDisableHistoryInTransactionalMode
  -- | Disable table module guard for read operations in local
  | FlagAllowReadInLocal
  -- | Disable emission of pact events
  | FlagDisablePactEvents
  -- | FlagEnforceKeyFormats
  deriving (Eq,Ord,Show,Enum,Bounded)

-- | Flag string representation
flagRep :: ExecutionFlag -> Text
flagRep = T.pack . drop 4 . show

-- | Flag string representations
flagReps :: Map Text ExecutionFlag
flagReps = M.fromList $ map go [minBound .. maxBound]
  where go f = (flagRep f,f)

-- From pact
-- | All of the types included in our evaluation environment.
data EvalEnv b i
  = EvalEnv
  { _eeMsgSigs :: Map PublicKeyText (Set (CapToken QualifiedName PactValue))
  -- ^ The list of provided keys and scoped capabilities
  , _eePactDb :: PactDb b i
  -- ^ The Pact database store
  , _eeMsgBody :: EnvData PactValue
  -- ^ Transaction-provided data
  , _eeHash :: Hash
  -- ^ The transaction hash
  , _eePublicData :: PublicData
  -- ^ Chain data provided to pact
  , _eeDefPactStep :: Maybe DefPactStep
  -- The (possible) defpact execution step
  , _eeMode :: ExecutionMode
  -- ^ The pact execution mode: local or transactional
  , _eeFlags :: Set ExecutionFlag
  -- ^ The present runtime flags in the eval environment
  , _eeNatives :: Map Text b
  -- ^ The native resolution map
  , _eeNamespacePolicy :: NamespacePolicy
  }

makeLenses ''EvalEnv

newtype PactState b i
  = PactState
  { _psLoaded :: Loaded b i
  }

makeLenses ''PactState

data StackFunctionType
  = SFDefun
  | SFDefcap
  | SFDefPact
  deriving (Eq, Show, Enum, Bounded)

data StackFrame
  = StackFrame
  { _sfFunction :: Text
  , _sfModule :: ModuleName
  , _sfFnType :: StackFunctionType }
  deriving Show

data EvalState b i
  = EvalState
  { _esCaps :: CapState QualifiedName PactValue
  , _esStack :: [StackFrame]
  , _esEvents :: [PactEvent PactValue]
  , _esLoaded :: Loaded b i
  , _esDefPactExec :: Maybe DefPactExec
  } deriving Show

instance Default (EvalState b i) where
  def = EvalState def [] [] mempty Nothing

makeClassy ''EvalState

instance HasLoaded (EvalState b i) b i where
  loaded = esLoaded

class (Monad m) => MonadEvalEnv b i m | m -> b, m -> i where
  readEnv :: m (EvalEnv b i)

-- | Our monad mirroring `EvalState` for our evaluation state
class Monad m => MonadEvalState b i m | m -> b, m -> i where
  getEvalState :: m (EvalState b i)
  putEvalState :: EvalState b i -> m ()
  modifyEvalState :: (EvalState b i -> EvalState b i) -> m ()

-- Our General constraint for evaluation and general analysis
type MonadEval b i m =
  ( MonadEvalEnv b i m
  , MonadEvalState b i m
  , MonadGas m
  , MonadError (PactError i) m
  , MonadIO m
  , Default i
  , Show i
  , Show b)

-- | A default evaluation environment meant for
--   uses such as the repl
defaultEvalEnv :: PactDb b i -> M.Map Text b -> EvalEnv b i
defaultEvalEnv pdb m
  = EvalEnv
  { _eeMsgSigs = mempty
  , _eePactDb = pdb
  , _eeMsgBody = EnvData mempty
  , _eeHash = defaultPactHash
  , _eePublicData = def
  , _eeDefPactStep = Nothing
  , _eeMode = Transactional
  , _eeFlags = mempty
  , _eeNatives = m
  , _eeNamespacePolicy = SimpleNamespacePolicy
  }