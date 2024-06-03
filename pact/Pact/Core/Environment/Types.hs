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
{-# LANGUAGE DeriveAnyClass #-}

module Pact.Core.Environment.Types
 ( EvalEnv(..)
 , eeMsgSigs, eePactDb
 , eeHash, eeMsgBody
 , eeDefPactStep, eeSPVSupport
 , eePublicData, eeMode, eeFlags
 , eeNatives, eeGasModel
 , eeNamespacePolicy, eeGasRef
 , eeMsgVerifiers
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
 , GasLogEntry(..)
 , RecursionCheck(..)
 , PactTrace(..)
 ) where


import Control.Lens
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Set(Set)
import Data.Text(Text)
import Data.Map.Strict(Map)
import Data.List.NonEmpty (NonEmpty(..))
import Data.IORef
import Data.Default
import System.Clock

import Control.DeepSeq
import GHC.Generics
import Control.Monad.Catch as Exceptions

import qualified Data.Text as T
import qualified Data.Map.Strict as M

import Pact.Core.Persistence.Types
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
import Pact.Core.StackFrame
import Pact.Core.Builtin (IsBuiltin)
import Pact.Core.Verifiers
import Pact.Core.SPV

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
  -- | Run the validity checks on keys
  | FlagEnforceKeyFormats
  -- | Require keysets to be defined in namespaces
  | FlagRequireKeysetNs
  -- | Flag disabling return type checking
  | FlagDisableRTC
  deriving (Eq,Ord,Show,Enum,Bounded, Generic)

instance NFData ExecutionFlag

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
  , _eeMsgVerifiers :: Map VerifierName (Set (CapToken QualifiedName PactValue))
  -- ^ The list of provided verifiers
  , _eePactDb :: PactDb b i
  -- ^ The Pact database store
  , _eeMsgBody :: PactValue
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
  -- ^ The implemented namespace policy
  , _eeGasRef :: IORef MilliGas
  -- ^ The gas ref
  , _eeGasModel :: GasModel b
  -- ^ The current gas model
  , _eeSPVSupport :: SPVSupport
  -- ^ The SPV backend
  } deriving (Generic)

instance (NFData b, NFData i) => NFData (EvalEnv b i)

makeLenses ''EvalEnv


data GasLogEntry b = GasLogEntry
  { _gleCause :: Either GasArgs b
  , _gleThisUsed :: MilliGas
  , _gleTotalUsed :: MilliGas
  } deriving (Show, Generic, NFData)

newtype RecursionCheck
  = RecursionCheck (Set QualifiedName)
  deriving (Show, Generic, NFData)

instance Default RecursionCheck where
  def = RecursionCheck mempty

data PactTrace b i
  = TraceFunctionEnter !TimeSpec (StackFrame i) i
  | TraceNativeEnter !TimeSpec b i
  | TraceFunctionExit !TimeSpec (StackFrame i) i
  | TraceNativeExit !TimeSpec b i
  deriving (Show, Generic)

instance (NFData b, NFData i) => NFData (PactTrace b i) where
  rnf = \case
    TraceFunctionEnter (TimeSpec s ns) b i ->
      rnf s `seq` rnf ns `seq` rnf b `seq` rnf i
    TraceNativeEnter (TimeSpec s ns) b i ->
      rnf s `seq` rnf ns `seq` rnf b `seq` rnf i
    TraceFunctionExit (TimeSpec s ns) b i ->
      rnf s `seq` rnf ns `seq` rnf b `seq` rnf i
    TraceNativeExit (TimeSpec s ns) b i ->
      rnf s `seq` rnf ns `seq` rnf b `seq` rnf i

-- | Interpreter mutable state.
data EvalState b i
  = EvalState
  { _esCaps :: !(CapState QualifiedName PactValue)
  -- ^ The current set of granted and installed
  -- capabilities
  , _esStack :: ![StackFrame i]
  -- ^ The runtime callstack, as a structure
  , _esEvents :: ![PactEvent PactValue]
  -- ^ The list of emitted pact events, if any
  , _esLoaded :: !(Loaded b i)
  -- ^ The runtime symbol table and module environment
  , _esDefPactExec :: !(Maybe DefPactExec)
  -- ^ The current defpact execution state, if any
  , _esGasLog :: !(Maybe [GasLogEntry b])
  -- ^ The current gas log
  , _esCheckRecursion :: NonEmpty RecursionCheck
    -- ^ Sequence of gas expendature events.
  , _esTraceOutput :: [PactTrace b i]
  } deriving (Show, Generic)

instance (NFData b, NFData i) => NFData (EvalState b i)

instance Default (EvalState b i) where
  def = EvalState def [] [] mempty Nothing Nothing (RecursionCheck mempty :| []) []

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
  , MonadError (PactError i) m
  , MonadIO m
  , Exceptions.MonadCatch m
  , Default i
  , Show i
  , IsBuiltin b
  , Show b)

-- | A default evaluation environment meant for
--   uses such as the repl
defaultEvalEnv :: PactDb b i -> M.Map Text b -> IO (EvalEnv b i)
defaultEvalEnv pdb m = do
  gasRef <- newIORef mempty
  pure $ EvalEnv
    { _eeMsgSigs = mempty
    , _eeMsgVerifiers = mempty
    , _eePactDb = pdb
    , _eeMsgBody = PObject mempty
    , _eeHash = defaultPactHash
    , _eePublicData = def
    , _eeDefPactStep = Nothing
    , _eeMode = Transactional
    , _eeFlags = mempty
    , _eeNatives = m
    , _eeNamespacePolicy = SimpleNamespacePolicy
    , _eeGasRef = gasRef
    , _eeGasModel = freeGasModel
    , _eeSPVSupport = noSPVSupport
    }