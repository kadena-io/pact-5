{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Pact.Core.Compile where

-- import Control.Lens
import Control.Monad.Except
-- import Data.Text as Text
import Data.Proxy
import Data.ByteString(ByteString)
-- import qualified Data.Map.Strict as Map
-- import qualified Data.Set as Set
import qualified Data.ByteString as B

import Pact.Core.Debug
import Pact.Core.Info
import Pact.Core.Persistence
-- import Pact.Core.Builtin
-- import Pact.Core.Gas
import Pact.Core.Names
import Pact.Core.Untyped.Utils
import Pact.Core.IR.Desugar
import Pact.Core.IR.Typecheck
-- import Pact.Core.Type
import Pact.Core.Typed.Overload
import Pact.Core.Errors
import Pact.Core.Pretty

-- import Pact.Core.Untyped.Eval.Runtime
-- import Pact.Core.Repl.Runtime
-- import Pact.Core.Repl.Runtime.ReplBuiltin

-- import qualified Pact.Core.IR.Term as IR
import qualified Pact.Core.Typed.Term as Typed
import qualified Pact.Core.Untyped.Term as Untyped
-- import qualified Pact.Core.Syntax.Lisp.ParseTree as Lisp

import qualified Pact.Core.Syntax.Lisp.LexUtils as Lisp
import qualified Pact.Core.Syntax.Lisp.Lexer as Lisp
import qualified Pact.Core.Syntax.Lisp.Parser as Lisp

import Debug.Trace

type HasCompileEnv raw reso m
  = ( MonadError PactErrorI m, DesugarBuiltin raw, TypeOfBuiltin raw
    , SolveOverload raw reso, Pretty raw, Pretty reso, PhaseDebug m)

_parseOnly
  :: ByteString -> Either PactErrorI [Lisp.ParsedTopLevel]
_parseOnly source = do
  lexed <- liftEither (Lisp.lexer source)
  traceM (show (Lisp._ptToken <$> lexed))
  liftEither (Lisp.parseProgram lexed)

_parseOnlyFile :: FilePath -> IO (Either PactErrorI [Lisp.ParsedTopLevel])
_parseOnlyFile fp = _parseOnly <$> B.readFile fp

compileTypedExprGen :: forall raw reso m
  . (HasCompileEnv raw reso m)
  => ByteString
  -> Proxy raw
  -> PactDb m reso LineInfo
  -> Loaded reso LineInfo
  -> m (Typed.Term Name NamedDeBruijn reso LineInfo, Loaded reso LineInfo)
compileTypedExprGen source proxy pactDb loaded = do
  lexed <- liftEither (Lisp.lexer source)
  debugPrint DebugLexer lexed
  parsed <- liftEither (Lisp.parseExpr lexed)
  debugPrint DebugParser parsed
  (DesugarOutput desugared loaded' _) <- runDesugarTermLisp proxy pactDb loaded parsed
  debugPrint DebugDesugar desugared
  (tys, typed) <- liftEither $ runInferTerm loaded' desugared
  debugPrint DebugTypechecker typed
  debugPrint DebugTypecheckerType tys
  o <- liftEither (runOverloadTerm typed)
  debugPrint DebugSpecializer o
  pure (o, loaded')

compileUntypedExprGen :: forall raw reso m
  . (HasCompileEnv raw reso m)
  => ByteString
  -> Proxy raw
  -> PactDb m reso LineInfo
  -> Loaded reso LineInfo
  -> m (Untyped.Term Name reso LineInfo, Loaded reso LineInfo)
compileUntypedExprGen source proxy pactDb loaded = do
  (typedTerm, l) <- compileTypedExprGen source proxy pactDb loaded
  let untyped = fromTypedTerm typedTerm
  debugPrint DebugUntyped untyped
  pure (untyped, l)


compileTypedExpr :: forall raw reso m
  . (HasCompileEnv raw reso m)
  => ByteString
  -> Proxy raw
  -> PactDb m reso LineInfo
  -> Loaded reso LineInfo
  -> m (Typed.Term Name NamedDeBruijn reso LineInfo, Loaded reso LineInfo)
compileTypedExpr source proxy pactDb loaded = do
  lexed <- liftEither (Lisp.lexer source)
  debugPrint DebugLexer lexed
  parsed <- liftEither (Lisp.parseExpr lexed)
  debugPrint DebugParser parsed
  (DesugarOutput desugared loaded' _) <- runDesugarTermLisp proxy pactDb loaded parsed
  debugPrint DebugDesugar desugared
  (tys, typed) <- liftEither $ runInferTermNonGen loaded' desugared
  debugPrint DebugTypechecker typed
  debugPrint DebugTypecheckerType tys
  o <- liftEither (runOverloadTerm typed)
  debugPrint DebugSpecializer o
  pure (o, loaded')

compileUntypedExpr :: forall raw reso m
  . (HasCompileEnv raw reso m)
  => ByteString
  -> Proxy raw
  -> PactDb m reso LineInfo
  -> Loaded reso LineInfo
  -> m (Untyped.Term Name reso LineInfo, Loaded reso LineInfo)
compileUntypedExpr source proxy pactDb loaded = do
  (typedTerm, l) <- compileTypedExprGen source proxy pactDb loaded
  let untyped = fromTypedTerm typedTerm
  debugPrint DebugUntyped untyped
  pure (untyped, l)


