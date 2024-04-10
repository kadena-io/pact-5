{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}


module Pact.Core.Repl.Utils
 ( ReplDebugFlag(..)
 , printDebug
 , ReplM(..)
 , replFlagSet
 , runReplT
 , ReplState(..)
 , replFlags
 , replPactDb
 , replGas
 , replEvalLog
 , replEvalEnv
 , replEvalState
 , replUserDocs
 , replTLDefPos
 , replNativesEnabled
 , whenReplFlagSet
 , unlessReplFlagSet
 , debugIfFlagSet
 , replCompletion
 , replCurrSource
 , replTx
 , ReplAction(..)
 , parseReplAction
 , prettyReplFlag
 , replError
 , SourceCode(..)
 ) where

import Control.Lens
import Control.Monad ( when, unless )
import Control.Monad.Reader
import Control.Monad.State.Strict(MonadState(..))
import Control.Monad.Catch
import Control.Monad.Except

import Data.Void
import Data.IORef
import Data.Set(Set)
import Data.Text(Text)
import Data.List(isPrefixOf)
import Data.Maybe(mapMaybe)
import Data.Map.Strict(Map)
import qualified Data.Set as Set
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Text.Megaparsec((<|>), (<?>))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP

import Pact.Core.Info
import Pact.Core.Names
import Pact.Core.Persistence
import Pact.Core.Pretty
import Pact.Core.Gas
import Pact.Core.Errors
import Pact.Core.Debug
import Pact.Core.Environment
import Pact.Core.Type
import qualified Pact.Core.IR.Term as Term
import qualified Pact.Core.Syntax.ParseTree as Syntax

import System.Console.Haskeline.Completion

data SourceCode
  = SourceCode
  { _scFileName :: String
  , _scPayload :: Text }
  deriving Show

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
  = ReplT { unReplT :: ExceptT (PactError SpanInfo) (ReaderT (IORef (ReplState b)) IO) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (IORef (ReplState b))
    , MonadThrow
    , MonadError (PactError SpanInfo)
    , MonadCatch
    , MonadMask)
  via (ExceptT (PactError SpanInfo) (ReaderT (IORef (ReplState b)) IO))

instance MonadState (ReplState b) (ReplM b)  where
  get = ReplT (ExceptT (Right <$> ReaderT readIORef))
  put rs = ReplT (ExceptT (Right <$> ReaderT (`writeIORef` rs)))


-- | Passed in repl environment
data ReplState b
  = ReplState
  { _replFlags :: Set ReplDebugFlag
  , _replPactDb :: PactDb b SpanInfo
  , _replEvalState :: EvalState b SpanInfo
  , _replEvalEnv :: EvalEnv b SpanInfo
  , _replGas :: IORef Gas
  , _replEvalLog :: IORef (Maybe [(Text, Gas)])
  , _replCurrSource :: SourceCode
  , _replUserDocs :: Map QualifiedName Text
  -- ^ Used by Repl and LSP Server, reflects the user
  --   annotated @doc string.
  , _replTLDefPos :: Map QualifiedName SpanInfo
  -- ^ Used by LSP Server, reflects the span information
  --   of the TL definitions for the qualified name.
  , _replTx :: Maybe (TxId, Maybe Text)
  , _replNativesEnabled :: Bool
  }

makeLenses ''ReplState

instance MonadEvalEnv b SpanInfo (ReplM b) where
  readEnv = use replEvalEnv

instance MonadEvalState b SpanInfo (ReplM b) where
  getEvalState = use replEvalState
  putEvalState es =
    replEvalState .= es
  modifyEvalState f =
    replEvalState %= f


instance HasEvalState (ReplState b) b SpanInfo where
  evalState = replEvalState

instance (Pretty b, Show b) => PhaseDebug b SpanInfo (ReplM b) where
  debugPrint dp term = do
    case dp of
      DPLexer -> whenReplFlagSet ReplDebugLexer $ liftIO $ do
        putStrLn "----------- Lexer output -----------------"
        print (pretty term)
      DPParser -> whenReplFlagSet ReplDebugParser $ case term of
        Syntax.TLTerm t ->
          liftIO $ do
            putStrLn "----------- Parser output ----------------"
            print (pretty t)
        _ -> pure ()
      DPDesugar -> whenReplFlagSet ReplDebugDesugar $ case term of
        Term.TLTerm t ->
          liftIO $ do
            putStrLn "----------- Desugar output ---------------"
            print (pretty t)
            print (show t)
            print $ "At span information: " <> show (view Term.termInfo t)
        _ -> pure ()


instance HasLoaded (ReplState b) b SpanInfo where
  loaded = evalState . esLoaded

data ReplAction
  = RASetFlag ReplDebugFlag
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
    setFlag <?> "asdf"
  setFlag =
    cmdKw "debug" *> ((RASetFlag <$> replFlag) <|> (RADebugAll <$ MP.chunk "all") <|> (RADebugNone <$ MP.chunk "none"))


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

replCompletion
  :: [Text]
  -- ^ natives
  -> CompletionFunc (ReplM b)
replCompletion natives =
  completeQuotedWord (Just '\\') "\"" listFiles $
  completeWord (Just '\\') filenameWordBreakChars $ \str -> do
    tlns <- uses (loaded . loToplevel) M.keys
    moduleNames <- uses (loaded . loModules) (fmap renderModuleName . M.keys)
    prefixedNames <- uses (loaded . loModules) toPrefixed
    let
      cmds = [":load", ":type", ":syntax", ":debug"]
      allNames = Set.fromList $ T.unpack <$> concat
        [tlns, moduleNames, prefixedNames, natives, cmds]
    pure $ simpleCompletion <$> Set.toList (Set.filter (str `isPrefixOf`) allNames)
  where
  defNames = \case
    ModuleData md _ ->
      Term.defName <$> Term._mDefs md
    InterfaceData iface _ ->
      fmap (_argName . Term._dcSpec) $ mapMaybe (preview Term._IfDConst) $ Term._ifDefns iface
  toPrefixed m =
    concat $ prefixF <$> M.toList m
  prefixF (mn, ems) = let
    dns = defNames ems
    in fmap ((renderModuleName mn <> ".") <>) dns

runReplT :: IORef (ReplState b) -> ReplM b a -> IO (Either (PactError SpanInfo) a)
runReplT env (ReplT act) = runReaderT (runExceptT act) env

replError
  :: SourceCode
  -> PactErrorI
  -> Text
replError (SourceCode srcFile src) pe =
  let file = T.pack srcFile
      srcLines = T.lines src
      pei = view peInfo pe
      -- Note: The startline is 0-indexed, but we want our
      -- repl to output errors which are 1-indexed.
      start = _liStartLine pei
      spanLen = _liEndLine pei - _liStartLine pei
      -- We want the padding to be the biggest line number we will show, which
      -- is endLine + 1
      maxPad = length (show (_liEndLine pei + 1)) + 1
      slice = withLine start maxPad $ take (max 1 spanLen) $ drop start srcLines
      -- Render ^^^ only in the column slice
      colMarker = T.replicate (maxPad+1) " " <> "| " <> T.replicate (_liStartColumn pei) " " <> T.replicate (max 1 (_liEndColumn pei - _liStartColumn pei)) "^"
      errRender = renderText pe
      fileErr = file <> ":" <> T.pack (show (_liStartLine pei + 1)) <> ":" <> T.pack (show (_liStartColumn pei)) <> ": "
  in T.unlines ([fileErr <> errRender] ++ slice ++ [colMarker])
  where
  padLeft t pad = T.replicate (pad - (T.length t)) " " <> t <> " "
  -- Zip the line number with the source text, and apply the number padding correctly
  withLine st pad lns = zipWith (\i e -> padLeft (T.pack (show i)) pad <> "| " <> e) [st+1..] lns
