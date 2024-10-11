{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
-- | Tests for the sizes of various values.

module Pact.Core.Test.SizeOfTests where

import Data.Default
import Control.Monad.IO.Class
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
import Pact.Core.PactValue
import Pact.Core.Serialise

tests :: TestTree
tests = testGroup "SizeOfTests" $
  [ testCase "int" $ do
      Right size <- getSize SizeOfV0 (1 :: Int)
      assertEqual "size should be 5" 5 size
  , testCase "string" $ do
      Right size <- getSize SizeOfV0 ("a" :: Text)
      -- Bytes of string + tag overhead
      assertEqual "size should be 5" 5 size
  , testCase "FieldKey" $ do
      Right size <- getSize SizeOfV0 (Field "a")
      assertEqual "size should be 5" 5 size
  , testCase "PactValue1" $ do
      Right size <- getSize SizeOfV0 (PInteger 1)
      -- The size of the integer (at least 8 bytes) + the tag overhead of PLiteral (1 byte)
      -- + tag overhead of PInteger (1 byte)
      assertEqual "size should be 40" 10 size
  , sizeOfSmallObject SizeOfV0 22
  ]

getSize :: SizeOf a => SizeOfVersion -> a -> IO (Either PactErrorI Bytes)
getSize version value = do
  pdb <- mockPactDb serialisePact_repl_spaninfo
  ee <- defaultEvalEnv pdb replBuiltinMap
  let es = def
  (v, _state) <- liftIO $ runEvalM (ExecEnv ee) es (sizeOf def version value)
  return v

sizeOfSmallObject :: SizeOfVersion -> Bytes -> TestTree
sizeOfSmallObject version expectation = testCase ("pactvalue-smallobject-" ++ show version) $ do
    Right size <- getSize version smallObject
    assertEqual "size should match expectation" expectation size
  where
    smallObject :: PactValue
    smallObject = PObject $ Map.fromList [(Field "a", PInteger 1)]
