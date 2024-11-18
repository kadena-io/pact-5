{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module Pact.Core.Repl.Utils
 ( ReplDebugFlag(..)
 , printDebug
 , replFlagSet
 , evalReplM
 , ReplState(..)
 , replFlags
 , replEvalEnv
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
 , renderReplFlag
 , replError
 , SourceCode(..)
 , useReplState
 , usesReplState
 , (.==)
 , (%==)
 , gasLogEntrytoPactValue
 , replPrintLn
 , replPrintLn'
 , recordTestSuccess
 , recordTestFailure
 ) where

import Control.Lens
import Control.Monad ( when, unless )
import Control.Monad.Reader

import Data.Void
import Data.IORef
import Data.Text(Text)
import Data.List(isPrefixOf)
import Data.Maybe(mapMaybe)
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
import Pact.Core.Errors
import Pact.Core.Environment
import Pact.Core.Type
import Pact.Core.Builtin
import Pact.Core.PactValue
import Pact.Core.Debug
import qualified Pact.Core.IR.Term as Term

import System.Console.Haskeline.Completion
import Data.Default

renderReplFlag :: ReplDebugFlag -> String
renderReplFlag = \case
  ReplDebugLexer -> "lexer"
  ReplDebugParser -> "parser"
  ReplDebugDesugar -> "desugar"

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
  (ReplDebugDesugar <$ MP.chunk "desugar")

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

replFlagSet
  :: ReplDebugFlag
  -> ReplM b Bool
replFlagSet flag =
  usesReplState replFlags (Set.member flag)

getReplState :: ReplM b (ReplState b)
getReplState = do
  r <- ask
  let (ReplEnv ref) = r
  liftIO $ readIORef ref

useReplState :: Lens' (ReplState b) s -> ReplM b s
useReplState l = do
  r <- ask
  let (ReplEnv ref) = r
  v <- liftIO $ readIORef ref
  pure (view l v)

usesReplState :: Lens' (ReplState b) s -> (s -> a) -> ReplM b a
usesReplState l f = do
  r <- ask
  let (ReplEnv ref) = r
  v <- liftIO $ readIORef ref
  pure (views l f v)

(.==) :: Lens' (ReplState b) s -> s -> ReplM b ()
l .== s = do
  r <- ask
  let (ReplEnv ref) = r
  liftIO (modifyIORef ref (set l s))

(%==) :: Lens' (ReplState b) s -> (s -> s) -> ReplM b ()
l %== f = do
  r <- ask
  let (ReplEnv ref) = r
  liftIO (modifyIORef ref (over l f))

infixr 4 .==, %==

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
      allNames = Set.fromList $ T.unpack <$> concat
        [tlns, moduleNames, prefixedNames, natives]
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

evalReplM :: IORef (ReplState b) -> ReplM b a -> IO (Either (PactError FileLocSpanInfo) a)
evalReplM env st = runEvalMResult (ReplEnv env) def st

replError
  :: (HasSpanInfo i, Pretty i)
  => SourceCode
  -> PactError i
  -> Text
replError (SourceCode srcFile src) pe =
  let file = T.pack srcFile
      srcLines = T.lines src
      pei = view (peInfo.spanInfo) pe
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
  in T.unlines ([fileErr <> errRender] ++ slice ++ [colMarker, sfRender])
  where
  sfRender = case viewErrorStack pe of
    [] -> mempty
    sfs ->
      let renderSf sf = "  at" <> pretty sf <> ":" <> pretty (_sfInfo sf)
      in renderText' $ vsep (renderSf <$> sfs)
  padLeft t pad = T.replicate (pad - (T.length t)) " " <> t <> " "
  -- Zip the line number with the source text, and apply the number padding correctly
  withLine st pad lns = zipWith (\i e -> padLeft (T.pack (show i)) pad <> "| " <> e) [st+1..] lns

gasLogEntrytoPactValue :: Pretty i => GasLogEntry (ReplBuiltin CoreBuiltin) i -> PactValue
gasLogEntrytoPactValue entry = PString $ renderCompactText' $ n <> ": " <> pretty (_gleThisUsed entry)
  where
    n = pretty (_gleArgs entry) <+> pretty (_gleInfo entry)

replPrintLn :: Pretty a => a -> EvalM 'ReplRuntime b FileLocSpanInfo ()
replPrintLn p = replPrintLn' (renderCompactText p)

replPrintLn' :: Text -> EvalM 'ReplRuntime b FileLocSpanInfo ()
replPrintLn' p = do
  r <- getReplState
  case _replLogType r of
    ReplStdOut -> _replOutputLine r p
    ReplLogOut v ->
      liftIO (modifyIORef' v (p:))

recordTestResult
  :: Text
  -- ^ Test name
  -> FileLocSpanInfo
  -- ^ Test location
  -> ReplTestStatus
  -> ReplM b ()
recordTestResult name loc status = do
  let testResult = ReplTestResult name loc status
  replTestResults %== (testResult :)

recordTestSuccess :: Text -> FileLocSpanInfo -> ReplM b ()
recordTestSuccess name loc = recordTestResult name loc ReplTestPassed

recordTestFailure :: Text -> FileLocSpanInfo -> Text -> ReplM b ()
recordTestFailure name loc failmsg = recordTestResult name loc (ReplTestFailed failmsg)

-- This orphan instance allows us to separate
-- the repl declaration out, as ugly as it is
instance DebugPrintable 'ReplRuntime (ReplBuiltin CoreBuiltin) where
  debugPrint dp term =
    ask >>= \case
      ReplEnv _ -> do
        case dp of
          DPLexer -> whenReplFlagSet ReplDebugLexer $ liftIO $ do
            putStrLn "----------- Lexer output -----------------"
            print (pretty term)
          DPParser -> whenReplFlagSet ReplDebugParser $
              liftIO $ do
                putStrLn "----------- Parser output ----------------"
                print (pretty term)
          DPDesugar -> whenReplFlagSet ReplDebugDesugar $ case term of
            Term.TLTerm t ->
              liftIO $ do
                putStrLn "----------- Desugar output ---------------"
                print (pretty t)
            _ -> pure ()
