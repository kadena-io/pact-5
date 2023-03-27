-- |

module Pact.Core.LSP.AnalyzeSource where

import Data.ByteString (ByteString)
import Pact.Core.Errors (PactErrorI)
import Control.Monad.Except (ExceptT, liftEither, withExceptT)

import qualified Pact.Core.Syntax.Lisp.Lexer as Lisp
import qualified Pact.Core.Syntax.Lisp.Parser as Lisp

data AnalyzeError
 = PactError PactErrorI

data AnalyzeResult
  = AnalyzeResult

analyzeSource :: Monad m => ByteString -> ExceptT AnalyzeError m AnalyzeResult
analyzeSource src = do
  compiled  <- withExceptT PactError $ do
    lexed <- liftEither (Lisp.lexer src)
    liftEither (Lisp.parseReplProgram lexed)
  pure AnalyzeResult
