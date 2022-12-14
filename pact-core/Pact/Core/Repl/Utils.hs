{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}


module Pact.Core.Repl.Utils
 ( ReplDebugFlag(..)
 , printDebug
 , ReplM(..)
 , replFlagSet
 , runReplT
 , ReplState(..)
 , replFlags
 , replLoaded
 , replPactDb
 , replGas
 , replEvalLog
 , whenReplFlagSet
 , unlessReplFlagSet
 , debugIfFlagSet
 , replCompletion
 , ReplAction(..)
 , parseReplAction
 , prettyReplFlag
 , ReplSource(..)
 ) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Catch
import Control.Monad.Except

import Data.Void
import Data.IORef
import Data.Set(Set)
import Data.Text(Text)
import Data.List(isPrefixOf)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Text.Megaparsec((<|>))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP

import Pact.Core.Info
import Pact.Core.Names
import Pact.Core.Persistence
import Pact.Core.Pretty
import Pact.Core.Gas
import Pact.Core.Errors
import qualified Pact.Core.Untyped.Term as Term

import System.Console.Haskeline.Completion

data ReplDebugFlag
  = ReplDebugLexer
  | ReplDebugParser
  | ReplDebugDesugar
  | ReplDebugTypechecker
  | ReplDebugTypecheckerType
  | ReplDebugSpecializer
  | ReplDebugUntyped
  deriving (Show, Eq, Ord, Enum, Bounded)

prettyReplFlag :: ReplDebugFlag -> String
prettyReplFlag = \case
  ReplDebugLexer -> "lexer"
  ReplDebugParser -> "parser"
  ReplDebugDesugar -> "desugar"
  ReplDebugTypechecker -> "tc-term"
  ReplDebugTypecheckerType -> "tc-type"
  ReplDebugSpecializer -> "specializer"
  ReplDebugUntyped -> "untyped-core"

newtype ReplM b a
  = ReplT { unReplT :: ExceptT (PactError LineInfo) (ReaderT (IORef (ReplState b)) IO) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadError (PactError LineInfo)
    , MonadCatch
    , MonadMask)
  via (ExceptT (PactError LineInfo) (ReaderT (IORef (ReplState b)) IO))


instance MonadState (ReplState b) (ReplM b)  where
  get = ReplT (ExceptT (Right <$> ReaderT readIORef))
  put rs = ReplT (ExceptT (Right <$> ReaderT (`writeIORef` rs)))

-- | Passed in repl environment
-- Todo: not a `newtype` since there's
-- more fields we can set.
data ReplState b
  = ReplState
  { _replFlags :: Set ReplDebugFlag
  , _replLoaded :: Loaded b LineInfo
  , _replPactDb :: PactDb (ReplM b) b LineInfo
  , _replGas :: IORef Gas
  , _replEvalLog :: IORef (Maybe [(Text, Gas)])
  }


makeLenses ''ReplState

data ReplAction
  = RALoad Text
  | RASetLispSyntax
  | RASetNewSyntax
  | RATypecheck Text
  | RASetFlag ReplDebugFlag
  | RADebugAll
  | RADebugNone
  | RAExecuteExpr Text
  deriving Show

type ReplParser = MP.Parsec Void Text

replFlag :: ReplParser ReplDebugFlag
replFlag =
  (ReplDebugLexer <$ MP.chunk "lexer") <|>
  (ReplDebugParser <$ MP.chunk "parser") <|>
  (ReplDebugDesugar <$ MP.chunk "desugar") <|>
  (ReplDebugTypechecker <$ MP.chunk "tc-term") <|>
  (ReplDebugTypecheckerType <$ MP.chunk "tc-type") <|>
  (ReplDebugSpecializer <$ MP.chunk "specializer") <|>
  (ReplDebugUntyped <$ MP.chunk "untyped-core")

replAction :: ReplParser ReplAction
replAction =
  cmd <|> execute
  where
  execute =
    RAExecuteExpr <$> MP.takeRest
  cmdKw kw = MP.chunk kw *> MP.space1
  cmd = do
    _ <- MP.chunk ":"
    load <|> setLang <|> setFlag <|> tc
  setFlag =
    cmdKw "debug" *> ((RASetFlag <$> replFlag) <|> (RADebugAll <$ MP.chunk "all") <|> (RADebugNone <$ MP.chunk "none"))
  setLang = do
    cmdKw "syntax"
    (RASetLispSyntax <$ MP.chunk "lisp") <|> (RASetNewSyntax <$ MP.chunk "new")
  tc = do
    cmdKw "type"
    RATypecheck <$> MP.takeRest
  load = do
    cmdKw "load"
    let c = MP.char '\"'
    RALoad <$> MP.between c c (MP.takeWhile1P Nothing (/= '\"'))

parseReplAction :: Text -> Maybe ReplAction
parseReplAction = MP.parseMaybe replAction

printDebug :: Pretty a => a -> ReplDebugFlag -> IO ()
printDebug a = \case
  ReplDebugLexer -> do
    putStrLn "----------- Lexer output -----------------"
    print (pretty a)
  ReplDebugParser -> do
    putStrLn "----------- Parser output ----------------"
    print (pretty a)
  ReplDebugDesugar -> do
    putStrLn "----------- Desugar output ---------------"
    print (pretty a)
  ReplDebugTypechecker -> do
    putStrLn "----------- Typechecker output -----------"
    print (pretty a)
  ReplDebugTypecheckerType -> do
    putStrLn "----------- Inferred type output ---------"
    print (pretty a)
  ReplDebugSpecializer -> do
    putStrLn "----------- Specializer output -----------"
    print (pretty a)
  ReplDebugUntyped -> do
    putStrLn "----------- Untyped core output ----------"
    print (pretty a)

replFlagSet
  :: ReplDebugFlag
  -> ReplM b Bool
replFlagSet flag =
  uses replFlags (Set.member flag)

debugIfFlagSet :: Pretty a => ReplDebugFlag -> a -> ReplM b ()
debugIfFlagSet flag a =
  whenReplFlagSet flag $ liftIO (printDebug a flag)

whenReplFlagSet :: ReplDebugFlag -> ReplM b () -> ReplM b ()
whenReplFlagSet flag ma =
  replFlagSet flag >>= \b -> when b ma

unlessReplFlagSet :: ReplDebugFlag -> ReplM b () -> ReplM b ()
unlessReplFlagSet flag ma =
  replFlagSet flag >>= \b -> unless b ma

data ReplSource
  = ReplSource
  { _rsFile :: Text
  , _rsSource :: Text
  } deriving Show

replCompletion
  :: [Text]
  -- ^ natives
  -> CompletionFunc (ReplM b)
replCompletion natives =
  completeQuotedWord (Just '\\') "\"" listFiles $
  completeWord (Just '\\') filenameWordBreakChars $ \str -> do
    tlns <- uses (replLoaded . loToplevel) Map.keys
    moduleNames <- uses (replLoaded . loModules) (fmap renderModuleName . Map.keys)
    prefixed <- uses (replLoaded . loModules) toPrefixed
    let
      cmds = [":load", ":type", ":syntax", ":debug"]
      allNames = Set.fromList $ T.unpack <$> concat
        [tlns, moduleNames, prefixed, natives, cmds]
    pure $ simpleCompletion <$> Set.toList (Set.filter (str `isPrefixOf`) allNames)
  where
  defNames = fmap Term.defName . Term._mDefs . _mdModule
  toPrefixed m =
    concat $ prefixF <$> Map.toList m
  prefixF (mn, ems) = let
    dns = defNames ems
    in fmap ((renderModuleName mn <> ".") <>) dns

runReplT :: IORef (ReplState b) -> ReplM b a -> IO (Either (PactError LineInfo) a)
runReplT env (ReplT act) = runReaderT (runExceptT act) env

