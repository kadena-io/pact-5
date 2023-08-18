{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Pact.Core.Repl.Types
( -- * Repl State
  ReplState(..)
, replLoaded
, replPactDb
, replGas
, replEvalLog
, replFlags

  -- * Repl monad
, ReplM(..)
, runReplM

 -- * Supported repl commmands
, DebugFlagUpdate(..)
, ReplAction(..)
, parseReplAction
, parseReplActionText
  -- * Repl debug flags

, ReplDebugFlag(..)
, prettyReplDebugFlag
, parseReplDebugFlag

  -- * Repl source files
, ReplSource(..)
) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Catch ( MonadCatch, MonadThrow, MonadMask )
import Control.Monad.Except

import Data.IORef
import Data.Set as Set
import Data.Text
import Data.Void


import Text.Megaparsec((<|>))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP

import Pact.Core.Info
import Pact.Core.Persistence
import Pact.Core.Gas
import Pact.Core.Errors
import Pact.Core.Debug


-- -------------------------------------------------------------------- --
-- Repl parser

-- | Repl Parser
--
type ReplParser = MP.Parsec Void Text

-- -------------------------------------------------------------------- --
-- Repl debug flags

-- | $Commands
--
-- The following repl commands are supported in Pact:
--
-- *
data ReplDebugFlag
  = ReplDebugLexer
  | ReplDebugParser
  | ReplDebugDesugar
  | ReplDebugTypechecker
  | ReplDebugTypecheckerType
  | ReplDebugSpecializer
  | ReplDebugUntyped
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Pretty print a debug flag
--
prettyReplDebugFlag :: ReplDebugFlag -> String
prettyReplDebugFlag = \case
  ReplDebugLexer -> "lexer"
  ReplDebugParser -> "parser"
  ReplDebugDesugar -> "desugar"
  ReplDebugTypechecker -> "tc-term"
  ReplDebugTypecheckerType -> "tc-type"
  ReplDebugSpecializer -> "specializer"
  ReplDebugUntyped -> "untyped-core"

-- | Parser for repl debug flags
--
parseReplDebugFlag :: ReplParser ReplDebugFlag
parseReplDebugFlag =
  (ReplDebugLexer <$ MP.chunk "lexer") <|>
  (ReplDebugParser <$ MP.chunk "parser") <|>
  (ReplDebugDesugar <$ MP.chunk "desugar") <|>
  (ReplDebugTypechecker <$ MP.chunk "tc-term") <|>
  (ReplDebugTypecheckerType <$ MP.chunk "tc-type") <|>
  (ReplDebugSpecializer <$ MP.chunk "specializer") <|>
  (ReplDebugUntyped <$ MP.chunk "untyped-core")

-- -------------------------------------------------------------------- --
-- Repl source

-- | Repl source
--
data ReplSource
  = ReplSource
  { _rsFile :: Text
  , _rsSource :: Text
  } deriving Show

-- -------------------------------------------------------------------- --
-- Repl monad

-- | Repl monad state
--
data ReplState b
  = ReplState
  { _replFlags :: Set ReplDebugFlag
  , _replLoaded :: Loaded b SpanInfo
  , _replPactDb :: PactDb b SpanInfo
  , _replGas :: IORef Gas
  , _replEvalLog :: IORef (Maybe [(Text, Gas)])
  }
makeLenses ''ReplState

instance PhaseDebug (ReplM b) where
  debugPrint _ _ = pure ()

instance HasLoaded (ReplState b) b SpanInfo where
  loaded = replLoaded

-- | The Pact repl monad
--
-- All expressions and flag updates run through this monad in the repl.
--
newtype ReplM b a
  = ReplM { unReplM :: ExceptT (PactError SpanInfo) (ReaderT (IORef (ReplState b)) IO) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadError (PactError SpanInfo)
    , MonadCatch
    , MonadMask)
  via (ExceptT (PactError SpanInfo) (ReaderT (IORef (ReplState b)) IO))

instance MonadState (ReplState b) (ReplM b)  where
  get = ReplM (ExceptT (Right <$> ReaderT readIORef))
  put rs = ReplM (ExceptT (Right <$> ReaderT (`writeIORef` rs)))

-- | Run repl action
--
runReplM :: IORef (ReplState b) -> ReplM b a -> IO (Either (PactError SpanInfo) a)
runReplM env (ReplM act) = runReaderT (runExceptT act) env

-- -------------------------------------------------------------------- --
-- Suported Repl Commands

-- | Debug flag delta type for setting debug flags
--
-- TODO: We can make this more robust by allowing for multiple flag updates
--
data DebugFlagUpdate
  = All
  | Some ReplDebugFlag
  | None
  deriving (Eq, Show)

-- | Parser for setting repl debug flags
--
parseReplDebugUpdate :: ReplParser DebugFlagUpdate
parseReplDebugUpdate =
  (All <$ MP.chunk "all")
  <|> (None <$ MP.chunk "none")
  <|> (Some <$> parseReplDebugFlag)

-- | Repl action dispatch
--
data ReplAction
  = RALoad Text
    -- ^ load a .pact or .repl file
  | RAShowHelp
    -- ^ show all commands and associated help
  -- | RATypecheck Text
    -- ^ typecheck an expression
  | RASetDebugFlag DebugFlagUpdate
    -- ^ set a repl debug flag
  | RAExecuteExpr Text
    -- ^ (default) execute an arbitrary pact expression
  deriving Show

parseReplAction :: ReplParser ReplAction
parseReplAction =
  cmd <|> execute
  where
  execute =
    RAExecuteExpr <$> MP.takeRest
  cmdKw kw = MP.chunk kw *> MP.space1
  cmd = do
    _ <- MP.char ':'
    load <|> setFlag <|> showHelp
  showHelp = pure RAShowHelp
  setFlag =
    cmdKw "debug" *> (RASetDebugFlag <$> parseReplDebugUpdate)

  -- tc = do
  --   cmdKw "type"
  --   RATypecheck <$> MP.takeRest
  load = do
    cmdKw "load"
    let c = MP.char '\"'
    RALoad <$> MP.between c c (MP.takeWhile1P Nothing (/= '\"'))

-- | Parser for Repl debug actions
--
parseReplActionText :: Text -> Maybe ReplAction
parseReplActionText = MP.parseMaybe parseReplAction
