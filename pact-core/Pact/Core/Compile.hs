{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Pact.Core.Compile where

-- import Control.Monad.IO.Class(MonadIO)
-- import Control.Monad.Except
-- import Data.Proxy
-- import Data.ByteString(ByteString)
-- import qualified Data.ByteString as B

-- import Pact.Core.Debug
-- import Pact.Core.Info
-- import Pact.Core.Persistence
-- import Pact.Core.Names
-- import Pact.Core.Untyped.Utils
-- import Pact.Core.IR.Desugar
-- import Pact.Core.IR.Typecheck
-- import Pact.Core.Typed.Overload
-- import Pact.Core.Errors
-- import Pact.Core.Pretty

-- import qualified Pact.Core.Typed.Term as Typed
-- import qualified Pact.Core.Untyped.Term as Untyped

-- import qualified Pact.Core.Syntax.Lisp.LexUtils as Lisp
-- import qualified Pact.Core.Syntax.Lisp.Lexer as Lisp
-- import qualified Pact.Core.Syntax.Lisp.Parser as Lisp

-- type HasCompileEnv raw reso m
--   = ( MonadError PactErrorI m, DesugarBuiltin raw, TypeOfBuiltin raw
--     , SolveOverload raw reso, Pretty raw, Pretty reso, PhaseDebug m
--     , MonadIO m)

-- _parseOnly
--   :: ByteString -> Either PactErrorI [Lisp.ParsedTopLevel]
-- _parseOnly source = do
--   lexed <- liftEither (Lisp.lexer source)
--   liftEither (Lisp.parseProgram lexed)

-- _parseOnlyFile :: FilePath -> IO (Either PactErrorI [Lisp.ParsedTopLevel])
-- _parseOnlyFile fp = _parseOnly <$> B.readFile fp


-- compileTypedExprGen
--   :: (HasCompileEnv raw reso m)
--   => ByteString
--   -> Proxy raw
--   -> PactDb m reso SpanInfo
--   -> Loaded reso SpanInfo
--   -> m (Typed.Term Name NamedDeBruijn reso SpanInfo, Loaded reso SpanInfo)
-- compileTypedExprGen source proxy pactDb loaded = do
--   lexed <- liftEither (Lisp.lexer source)
--   debugPrint DebugLexer lexed
--   parsed <- liftEither (Lisp.parseExpr lexed)
--   debugPrint DebugParser parsed
--   (DesugarOutput desugared loaded' _) <- runDesugarTermLisp proxy pactDb loaded parsed
--   debugPrint DebugDesugar desugared
--   (tys, typed) <- liftEither $ runInferTerm loaded' desugared
--   debugPrint DebugTypechecker typed
--   debugPrint DebugTypecheckerType tys
--   o <- liftEither (runOverloadTerm typed)
--   debugPrint DebugSpecializer o
--   pure (o, loaded')

-- compileUntypedExprGen
--   :: (HasCompileEnv raw reso m)
--   => ByteString
--   -> Proxy raw
--   -> PactDb m reso SpanInfo
--   -> Loaded reso SpanInfo
--   -> m (Untyped.Term Name reso SpanInfo, Loaded reso SpanInfo)
-- compileUntypedExprGen source proxy pactDb loaded = do
--   (typedTerm, l) <- compileTypedExprGen source proxy pactDb loaded
--   let untyped = fromTypedTerm typedTerm
--   debugPrint DebugUntyped untyped
--   pure (untyped, l)

-- compileTypedExpr :: forall raw reso m
--   . (HasCompileEnv raw reso m)
--   => ByteString
--   -> Proxy raw
--   -> PactDb m reso SpanInfo
--   -> Loaded reso SpanInfo
--   -> m (Typed.Term Name NamedDeBruijn reso SpanInfo, Loaded reso SpanInfo)
-- compileTypedExpr source proxy pactDb loaded = do
--   lexed <- liftEither (Lisp.lexer source)
--   debugPrint DebugLexer lexed
--   parsed <- liftEither (Lisp.parseExpr lexed)
--   debugPrint DebugParser parsed
--   (DesugarOutput desugared loaded' _) <- runDesugarTermLisp proxy pactDb loaded parsed
--   debugPrint DebugDesugar desugared
--   (tys, typed) <- liftEither $ runInferTermNonGen loaded' desugared
--   debugPrint DebugTypechecker typed
--   debugPrint DebugTypecheckerType tys
--   o <- liftEither (runOverloadTerm typed)
--   debugPrint DebugSpecializer o
--   pure (o, loaded')

-- compileUntypedExpr :: forall raw reso m
--   . (HasCompileEnv raw reso m)
--   => ByteString
--   -> Proxy raw
--   -> PactDb m reso SpanInfo
--   -> Loaded reso SpanInfo
--   -> m (Untyped.Term Name reso SpanInfo, Loaded reso SpanInfo)
-- compileUntypedExpr source proxy pactDb loaded = do
--   (typedTerm, l) <- compileTypedExprGen source proxy pactDb loaded
--   let untyped = fromTypedTerm typedTerm
--   debugPrint DebugUntyped untyped
--   pure (untyped, l)


