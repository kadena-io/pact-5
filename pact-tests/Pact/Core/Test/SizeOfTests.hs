-- | Tests for the sizes of various values.

module Pact.Core.Test.SizeOfTests where

import Data.Default
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map as Map
import Data.Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import Pact.Core.Builtin
import Pact.Core.Persistence.MockPersistence
import Pact.Core.Names
import Pact.Core.SizeOf
import Pact.Core.Errors
import Pact.Core.Environment.Types
import Pact.Core.Environment.Utils
import Pact.Core.Gas
import Pact.Core.PactValue
import Pact.Core.Serialise
import Pact.Core.IR.Eval.Runtime.Types

tests :: TestTree
tests = testGroup "SizeOfTests"
  [ testCase "int" $ do
      Right size <- getSize SizeOfV0 (1 :: Int)
      assertEqual "size should be 1" 16 size
  , testCase "string" $ do
      Right size <- getSize SizeOfV0 ("a" :: Text)
      assertEqual "size should be 3" 50 size
  , testCase "FieldKey" $ do
      Right size <- getSize SizeOfV0 (Field "a")
      assertEqual "size should be 50" 50 size
  , testCase "PactValue1" $ do
      Right size <- getSize SizeOfV0 (PInteger 1)
      assertEqual "size should be 32" 32 size
  , sizeOfSmallObject SizeOfV0 146
  , sizeOfSmallObject SizeOfV1 154
  ]

getSize :: SizeOf a => SizeOfVersion -> a -> IO (Either PactErrorI Bytes)
getSize version value = do
  pdb <- mockPactDb serialisePact_repl_spaninfo
  ee <- defaultEvalEnv pdb replCoreBuiltinMap
  let es = def
  (v, _state) <- liftIO $ runEvalM ee es (sizeOf version value)
  return v

sizeOfSmallObject :: SizeOfVersion -> Bytes -> TestTree
sizeOfSmallObject version expectation = testCase ("pactvalue-smallobject-" ++ show version) $ do
    Right size <- getSize version smallObject
    assertEqual "size should match expectation" expectation size
  where
    smallObject :: PactValue
    smallObject = PObject $ Map.fromList [(Field "a", PInteger 1)]

