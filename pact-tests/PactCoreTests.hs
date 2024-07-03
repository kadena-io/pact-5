module Main where

import Test.Tasty

import qualified Pact.Core.Test.CommandTests as CommandTests
import qualified Pact.Core.Test.ReplTests as ReplTests
import qualified Pact.Core.Test.LexerParserTests as LexerParserTests
import qualified Pact.Core.Test.LexerTests as LexerTests
import qualified Pact.Core.Test.PersistenceTests as PersistenceTests
import qualified Pact.Core.Test.SerialiseTests as SerialiseTests
import qualified Pact.Core.Test.LegacySerialiseTests as LegacySerialiseTests
import qualified Pact.Core.Test.StaticErrorTests as StaticErrorTests
import qualified Pact.Core.Test.ZkTests as ZkTests
import qualified Pact.Core.Test.PoseidonTests as PoseidonTests
import qualified Pact.Core.Test.LanguageServer as LanguageServer
import qualified Pact.Core.Test.GasGolden as GasGolden
import qualified Pact.Core.Test.SizeOfTests as SizeOfTests
import qualified Pact.Core.Test.ConTagGolden as ConTagGoldenTests

main :: IO ()
main = do
  replTests <- ReplTests.tests
  gasGolden <- GasGolden.tests
  legacyTests <- LegacySerialiseTests.tests
  commandTests <- CommandTests.tests
  defaultMain $ testGroup "pactTests"
    [ replTests
    , LexerTests.tests
    , LexerParserTests.tests
    , SerialiseTests.tests
    , legacyTests
    , StaticErrorTests.tests
    , ZkTests.tests
    , PoseidonTests.tests
    , PersistenceTests.tests
    , LanguageServer.tests
    , gasGolden
    , SizeOfTests.tests
    , commandTests
    , ConTagGoldenTests.tests
    ]

