-- |
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Core.LSP.AnalyzeSource where

import Data.ByteString (ByteString)
import Pact.Core.Errors (PactErrorI)
import Control.Monad.Except (ExceptT, liftEither, withExceptT,
                             MonadError, runExceptT)
import qualified Pact.Core.Syntax.Lisp.Lexer as Lisp
import qualified Pact.Core.Syntax.Lisp.Parser as Lisp
import Pact.Core.IR.Desugar (DesugarOutput(..), runDesugarReplTopLevel)
import Pact.Core.Builtin (ReplRawBuiltin, ReplCoreBuiltin)
import Data.Data (Proxy(..))
import qualified Pact.Core.Syntax.Lisp.ParseTree as Lisp
import Pact.Core.Info (SpanInfo)
import Pact.Core.Persistence (Loaded, PactDb, mockPactDb)
import Pact.Core.IR.Typecheck
import Control.Monad.State
import Control.Lens
import Pact.Core.Typed.Term (OverloadedReplTopLevel)
import Pact.Core.Names (NamedDeBruijn)
import Debug.Trace (trace)

data AnalyzeError
 = AnalyzeError PactErrorI

data AnalyzeResult
  = AnalyzeResult
  { _arCompiled :: [OverloadedReplTopLevel NamedDeBruijn ReplRawBuiltin SpanInfo]
  }

makeLenses ''AnalyzeResult

newtype AnalyzeM a
  = AnalyzeM { unAnalyzeM :: ExceptT AnalyzeError (StateT AnalyzeState IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError AnalyzeError,
            MonadState AnalyzeState)

data AnalyzeState
  = AnalyzeState
  { _loaded :: Loaded ReplCoreBuiltin SpanInfo
  }

makeLenses ''AnalyzeState


runAnalyze :: AnalyzeM a -> IO (Either AnalyzeError a, AnalyzeState)
runAnalyze (AnalyzeM a) = runStateT (runExceptT a) (AnalyzeState mempty)

analyzeSource :: ByteString -> AnalyzeM AnalyzeResult
analyzeSource src = do
  pactdb <- liftIO mockPactDb
  parsed  <- AnalyzeM . withExceptT AnalyzeError $ do
    lexed <- liftEither (Lisp.lexer src)
    liftEither (Lisp.parseReplProgram lexed)
  compiled <- traverse (compile pactdb) parsed
  pure (AnalyzeResult compiled)
  where
    compile :: PactDb (ExceptT PactErrorI (StateT AnalyzeState IO)) ReplCoreBuiltin SpanInfo
      -> Lisp.ReplSpecialTL SpanInfo
      -> AnalyzeM (OverloadedReplTopLevel NamedDeBruijn ReplRawBuiltin SpanInfo)
    compile pactdb = \case
      Lisp.RTL rtl -> compileRTL pactdb rtl
      Lisp.RTLReplSpecial rsf -> do
        -- handle load of file
        trace (show rsf) $ pure ()
        error "not implemented"
    compileRTL :: PactDb (ExceptT PactErrorI (StateT AnalyzeState IO)) ReplCoreBuiltin SpanInfo
      -> Lisp.ReplTopLevel SpanInfo
      -> AnalyzeM (OverloadedReplTopLevel NamedDeBruijn ReplRawBuiltin SpanInfo)
    compileRTL pactdb tl = do
      l <- use loaded
      AnalyzeM . withExceptT AnalyzeError $ do
        (DesugarOutput desugared loaded' _deps) <-
          runDesugarReplTopLevel (Proxy @ReplRawBuiltin) pactdb l tl
        (typechecked, loadedWithTc) <- liftEither (runInferReplTopLevel loaded' desugared)
        loaded .= loadedWithTc
        -- overloaded <- liftEither (runOverloadReplTopLevel typechecked)
        pure typechecked
