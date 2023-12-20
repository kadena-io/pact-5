{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}


module Pact.Core.Evaluate
 ( compileOnly
 , compileOnlyTerm
 , evaluate) where

import Control.Monad
import Control.Monad.Except
import Data.Bifunctor(bimap)
import Data.Text (Text)

import Pact.Core.Builtin
import Pact.Core.Compile
import Pact.Core.Environment
import Pact.Core.Errors
import Pact.Core.IR.Eval.RawBuiltin
import Pact.Core.IR.Eval.Runtime
import qualified Pact.Core.Syntax.Lexer as Lisp
import qualified Pact.Core.Syntax.Parser as Lisp
import qualified Pact.Core.Syntax.ParseTree as Lisp

compileOnly :: Text -> Either (PactError ()) [Lisp.TopLevel ()]
compileOnly =
  bimap void (fmap void) . (Lisp.lexer >=> Lisp.parseProgram)

compileOnlyTerm :: Text -> Either (PactError ()) (Lisp.Expr ())
compileOnlyTerm =
  bimap void void . (Lisp.lexer >=> Lisp.parseExpr)

evaluate
  :: EvalEnv RawBuiltin ()
  -> EvalState RawBuiltin ()
  -> Text
  -> IO (Either (PactError ()) [CompileValue ()],
         EvalState RawBuiltin ())
evaluate evalEnv es source = runEvalM evalEnv es $ do
  let builtins = rawBuiltinEnv @CEKBigStep
  parsed <- liftEither $ compileOnly source
  traverse (interpretTopLevel builtins) parsed
