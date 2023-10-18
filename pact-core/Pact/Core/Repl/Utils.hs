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
 , replPactDb
 , replGas
 , replEvalLog
 , replEvalEnv
 , replEvalState
 , whenReplFlagSet
 , unlessReplFlagSet
 , debugIfFlagSet
 , replCompletion
 , replCurrSource
 , replTx
 , ReplAction(..)
 , parseReplAction
 , prettyReplFlag
 , ReplSource(..)
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
import Data.ByteString(ByteString)
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
import qualified Pact.Core.IR.Term as Term
import qualified Pact.Core.Syntax.ParseTree as Syntax

import System.Console.Haskeline.Completion

newtype SourceCode
  = SourceCode ByteString
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
    , MonadThrow
    , MonadError (PactError SpanInfo)
    , MonadCatch
    , MonadMask)
  via (ExceptT (PactError SpanInfo) (ReaderT (IORef (ReplState b)) IO))


instance MonadState (ReplState b) (ReplM b)  where
  get = ReplT (ExceptT (Right <$> ReaderT readIORef))
  put rs = ReplT (ExceptT (Right <$> ReaderT (`writeIORef` rs)))

-- | Passed in repl environment
-- Todo: not a `newtype` since there's
-- more fields we can set.
data ReplState b
  = ReplState
  { _replFlags :: Set ReplDebugFlag
  -- , _replLoaded :: Loaded b SpanInfo
  , _replPactDb :: PactDb b SpanInfo
  , _replEvalState :: EvalState b SpanInfo
  , _replEvalEnv :: EvalEnv b SpanInfo
  , _replGas :: IORef Gas
  , _replEvalLog :: IORef (Maybe [(Text, Gas)])
  , _replCurrSource :: SourceCode
  , _replTx :: Maybe (TxId, Maybe Text)
  }


makeLenses ''ReplState

instance HasEvalState (ReplState b) b SpanInfo where
  evalState = replEvalState

instance Pretty b => PhaseDebug b i (ReplM b) where
  debugPrint dp term = do
    -- flags <- use replFlags
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
        _ -> pure ()


instance HasLoaded (ReplState b) b SpanInfo where
  loaded = evalState . esLoaded

data ReplAction
  -- = RALoad Text
  -- | RASetLispSyntax
  -- | RASetNewSyntax
  -- | RATypecheck Text
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
  -- setLang = do
  --   cmdKw "syntax"
  --   (RASetLispSyntax <$ MP.chunk "lisp") <|> (RASetNewSyntax <$ MP.chunk "new")
  -- tc = do
  --   cmdKw "type"
  --   RATypecheck <$> MP.takeRest
  -- load = do
  --   cmdKw "load"
  --   let c = MP.char '\"'
  --   RALoad <$> MP.between c c (MP.takeWhile1P Nothing (/= '\"'))

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
      fmap Term._dcName $ mapMaybe (preview Term._IfDConst) $ Term._ifDefns iface
    -- fmap Term.defName . Term._mDefs . _mdModule
  toPrefixed m =
    concat $ prefixF <$> M.toList m
  prefixF (mn, ems) = let
    dns = defNames ems
    in fmap ((renderModuleName mn <> ".") <>) dns

runReplT :: IORef (ReplState b) -> ReplM b a -> IO (Either (PactError SpanInfo) a)
runReplT env (ReplT act) = runReaderT (runExceptT act) env

replError
  :: ReplSource
  -> PactErrorI
  -> Text
replError (ReplSource file src) pe =
  let srcLines = T.lines src
      pei = view peInfo pe
      end = _liEndLine pei - _liStartLine pei
      slice = withLine (_liStartLine pei) $ take (max 1 end) $ drop (_liStartLine pei) srcLines
      colMarker = "  | " <> T.replicate (_liStartColumn pei) " " <> T.replicate (max 1 (_liEndColumn pei - _liStartColumn pei)) "^"
      errRender = renderText pe
      fileErr = file <> ":" <> T.pack (show (_liStartLine pei + 1)) <> ":" <> T.pack (show (_liStartColumn pei)) <> ": "
  in T.unlines ([fileErr <> errRender] ++ slice ++ [colMarker])
  where
  withLine st lns = zipWith (\i e -> T.pack (show i) <> " | " <> e) [st ..] lns
