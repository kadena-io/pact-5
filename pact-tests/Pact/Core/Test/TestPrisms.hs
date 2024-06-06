
{-# LANGUAGE TemplateHaskell #-}

module Pact.Core.Test.TestPrisms where

import Control.Lens
import Pact.Core.Errors

makePrisms ''LexerError
makePrisms ''ParseError
makePrisms ''DesugarError
makePrisms ''EvalError
makePrisms ''UserRecoverableError
makePrisms ''PactError
