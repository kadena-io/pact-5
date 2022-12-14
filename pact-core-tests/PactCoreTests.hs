module Main where

import Test.Tasty

import qualified Pact.Core.Test.Repl as ReplTests

main :: IO ()
main = do
  replTests <- ReplTests.tests
  defaultMain $ testGroup "pactTests"
    [ replTests
    ]
