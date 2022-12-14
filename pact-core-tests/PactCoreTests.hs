module Main where

import Test.Tasty

import qualified Pact.Core.Test.ReplTests as ReplTests

main :: IO ()
main = do
  replTests <- ReplTests.tests
  defaultMain $ testGroup "pactTests"
    [ replTests
    ]
