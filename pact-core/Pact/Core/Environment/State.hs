-- |
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Core.Environment.State
  ( EvalState(..)
  , MonadEvalState(..)
  , HasEvalState

  , Loaded(..)
  , esLoaded
  , HasLoaded(..)
  , ModuleData(..)
  , StackFrame(..)
  , StackFunctionType(..)
  , TxId(..)
  ) where

import Control.Lens (makeClassy)
import Control.Applicative ((<|>))
import Data.Default (Default, def)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Word (Word64)

import Pact.Core.Capabilities (CapState, PactEvent)
import Pact.Core.DefPacts.Types (DefPactExec)
import Pact.Core.IR.Term (Def, EvalDef, EvalInterface, EvalModule)
import Pact.Core.Names (FullyQualifiedName, ModuleName, Name, QualifiedName)
import Pact.Core.Namespace (Namespace)
import Pact.Core.PactValue (PactValue)
import Pact.Core.Type (DefKind, Type)


newtype TxId = TxId { _txId :: Word64 }
    deriving (Eq,Ord, Show)


-- | Modules as they are stored
-- in our backend.
-- That is: All module definitions, as well as their dependencies
-- Todo: bikeshed this name? This contains interface data
data ModuleData b i
  = ModuleData (EvalModule b i) (Map FullyQualifiedName (EvalDef b i))
  | InterfaceData (EvalInterface b i) (Map FullyQualifiedName (EvalDef b i))
  deriving (Show, Eq, Functor)


data StackFrame
  = StackFrame
  { _sfFunction :: Text
  , _sfModule :: ModuleName
  , _sfFnType :: StackFunctionType }
  deriving Show

data StackFunctionType
  = SFDefun
  | SFDefcap
  | SFDefPact
  deriving (Eq, Show, Enum, Bounded)


-- | Our loaded modules, names in top-level scope and fully qualified dependencies.
data Loaded b i
  = Loaded
  { _loModules :: Map ModuleName (ModuleData b i)
  -- ^ All loaded modules and interfaces
  , _loToplevel :: Map Text (FullyQualifiedName, DefKind)
  -- ^ All names bound @ the top level scope, that is, by `(use)` statements
  -- or module loads
  , _loNamespace :: Maybe Namespace
  -- ^ The potentially loaded current namespace
  , _loAllLoaded :: Map FullyQualifiedName (Def Name Type b i)
  -- ^ All of our fully qualified dependencies
  } deriving Show

makeClassy ''Loaded

instance Semigroup (Loaded b i) where
  (Loaded ms tl ns al) <> (Loaded ms' tl' ns' al') =
    Loaded (ms <> ms') (tl <> tl') (ns <|> ns') (al <> al')

instance Monoid (Loaded b i) where
  mempty = Loaded mempty mempty Nothing mempty

instance Default (Loaded b i) where
  def = Loaded mempty mempty Nothing mempty


-- | Aspects of the evaluation environment that are updated during the evaluation
-- of a single pact transaction.
data EvalState b i
  = EvalState
  { _esCaps :: CapState QualifiedName PactValue
  , _esStack :: [StackFrame]
  , _esEvents :: [PactEvent PactValue]
  , _esLoaded :: Loaded b i
  , _esDefPactExec :: Maybe DefPactExec
  , _esTxId :: Maybe TxId
  } deriving Show

instance Default (EvalState b i) where
  def = EvalState def [] [] mempty Nothing Nothing

makeClassy ''EvalState

-- | Any monad that can read, put, or modify the eval state.
class Monad m => MonadEvalState b i m | m -> b, m -> i where
  getEvalState :: m (EvalState b i)
  putEvalState :: EvalState b i -> m ()
  modifyEvalState :: (EvalState b i -> EvalState b i) -> m ()
