{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}


module Pact.Core.Evaluate where

import Control.Monad.Except
import Data.Text (Text)

import Pact.Core.Builtin
import Pact.Core.Compile
import Pact.Core.Environment
import Pact.Core.Errors
import Pact.Core.Info
import Pact.Core.IR.Eval.RawBuiltin
import Pact.Core.IR.Eval.Runtime
import qualified Pact.Core.Syntax.Lexer as Lisp
import qualified Pact.Core.Syntax.Parser as Lisp

evaluate
  :: EvalEnv RawBuiltin SpanInfo
  -> EvalState RawBuiltin SpanInfo
  -> Text
  -> IO (Either (PactError SpanInfo) [CompileValue SpanInfo],
         EvalState RawBuiltin SpanInfo)
evaluate evalEnv es source = runEvalM evalEnv es $ do
  let builtins = rawBuiltinEnv @CEKBigStep
  lexx <- liftEither (Lisp.lexer source)
  parsed <- liftEither $ Lisp.parseProgram lexx
  traverse (interpretTopLevel builtins) parsed
