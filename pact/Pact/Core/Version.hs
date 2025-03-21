-- |
-- Module      :  Pact.Core.Version
-- Copyright   :  (C) 2022 Kadena
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jose Cardona <jose@kadena.io>
--
-- Workaround module replacing PackageInfo_pact_tng
-- to fix the wonky shit in cabal 3.12
--


module Pact.Core.Version (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))

name :: String
name = "pact_tng"
version :: Version
version = Version [5,2] []

synopsis :: String
synopsis = "Smart contract language library and REPL"
copyright :: String
copyright = "Copyright (C) 2022 Kadena"
homepage :: String
homepage = "https://github.com/kadena-io/pact-5"
