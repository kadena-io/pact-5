{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
-- | Tests for the sizes of various values.

module Pact.Core.Test.SizeOfTests where

import Data.Default
import Control.Monad.IO.Class
import qualified Data.Map as Map
import Data.Text
import GHC.Generics (Generic)
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
      assertEqual "size should be 1" 16 size
  , testCase "string" $ do
      Right size <- getSize SizeOfV0 ("a" :: Text)
      assertEqual "size should be 3" 50 size
  , testCase "FieldKey" $ do
      Right size <- getSize SizeOfV0 (Field "a")
      assertEqual "size should be 50" 50 size
  , testCase "PactValue1" $ do
      Right size <- getSize SizeOfV0 (PInteger 1)
      assertEqual "size should be 40" 40 size
  , sizeOfSmallObject SizeOfV0 154
  , sizeOfGenericsTest SizeOfV0
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

-- Testing whether derived instance for empty constructors is 1 word
data A = A1 | A2 deriving (Eq, Show, Generic)
data B = B1 Int | B2 Int Bool | B3 Int Bool A deriving (Eq, Show, Generic)
data C a = C a deriving (Eq, Show, Generic)

instance SizeOf A
instance SizeOf B
instance SizeOf a => SizeOf (C a)

newtype D = D Int
  deriving (Eq, Show, Generic)

instance SizeOf D

newtype F = F Int
  deriving (Eq, Show, SizeOf)

sizeOfGenericsTest :: SizeOfVersion -> TestTree
sizeOfGenericsTest szVer =
  testCase ("SizeOf " <> show szVer <> " generics conform to specification") $ do
    Right a1Size <- getSize szVer A1
    Right a2Size <- getSize szVer A2
    assertEqual "A1 wordSize" wordSize a1Size
    assertEqual "A2 wordSize" wordSize a2Size

    Right b1Size <- getSize szVer (B1 0)
    Right b2Size <- getSize szVer (B2 0 True)
    Right b3Size <- getSize szVer (B3 0 True A1)
    Right intSize <- getSize szVer (0::Int)
    Right boolSize <- getSize szVer True
    assertEqual "B1 size" (intSize + constructorCost 1) b1Size
    assertEqual "B2 size" (intSize + boolSize + constructorCost 2) b2Size
    assertEqual "B3 size" (intSize + boolSize + a1Size + constructorCost 3) b3Size

    Right cSize <- getSize szVer (C (B1 0))
    Right dSize <- getSize szVer (D 1)
    assertEqual "C size" (b1Size + constructorCost 1) cSize
    assertEqual "D size" (intSize + constructorCost 1) dSize

    Right fSize <- getSize szVer (F 1)
    assertEqual "F size" intSize fSize
