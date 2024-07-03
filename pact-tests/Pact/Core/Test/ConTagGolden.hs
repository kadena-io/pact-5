
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :  Pact.Core.DeriveConTag
-- Copyright   :  (C) 2024 Kadena
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jose Cardona <jose@kadena.io>
--
-- Tests to ensure we don't change constructor tag orderings derived by
-- `DeriveConTag.hs`
--


module Pact.Core.Test.ConTagGolden where

import Data.Typeable
import Test.Tasty
import Test.Tasty.Golden
import System.FilePath
import Pact.Core.DeriveConTag
import Pact.JSON.Encode
import Pact.Core.Errors
import Pact.Core.Builtin

tests :: TestTree
tests = testGroup "ConTag Goldens"
  [ conTagGolden $ Proxy @LexerError
  , conTagGolden $ Proxy @ParseError
  , conTagGolden $ Proxy @DesugarError
  , conTagGolden $ Proxy @EvalError
  , conTagGolden $ Proxy @UserRecoverableError
  , conTagGolden $ Proxy @(PactError ())
  , conTagGolden $ Proxy @CoreBuiltin
  ]

conTagGoldensDir :: FilePath
conTagGoldensDir = "pact-tests" </> "constructor-tag-goldens"

conTagGolden :: forall a. (Typeable a, HasConstrInfo a) => Proxy a -> TestTree
conTagGolden p =
  let tyn = filter (/= ' ') $ show (typeOf (undefined :: a))
      fp = conTagGoldensDir </> (tyn <> ".golden")
  in goldenVsStringDiff (tyn <> " Golden") runDiff fp mkGolden
  where
  mkGolden =
    pure $ encode $ Array (allConstrInfos p)
  runDiff = \ref new -> ["diff", "-u", ref, new]
