module Main where

import Test.Tasty

import qualified Pact.Core.Test.ReplTests as ReplTests
import qualified Pact.Core.Test.LexerParserTests as LexerParserTests
import qualified Pact.Core.Test.LexerTests as LexerTests
import qualified Pact.Core.Test.ZkTests as ZkTests
import qualified Pact.Core.Test.PoseidonTests as PoseidonTests

main :: IO ()
main = do
  replTests <- ReplTests.tests
  defaultMain $ testGroup "pactTests"
    [ replTests
    , LexerTests.tests
    , LexerParserTests.tests
    , ZkTests.tests
    , PoseidonTests.tests
    ]
