{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE InstanceSigs #-}


module Pact.Core.Environment
 ( module Pact.Core.Environment.Types
 , module Pact.Core.Environment.Utils
 ) where

import Pact.Core.Environment.Types
import Pact.Core.Environment.Utils

-- import Control.Lens
-- import Control.Monad.Except
-- import Control.Monad.IO.Class
-- import Data.Set(Set)
-- import Data.Text(Text)
-- import Data.Map.Strict(Map)
-- import Data.Default

-- import qualified Data.Text as T
-- import qualified Data.Map.Strict as M

-- import Pact.Core.Persistence
-- import Pact.Core.Capabilities
-- import Pact.Core.Guards
-- import Pact.Core.PactValue ( PactValue, EnvData )
-- import Pact.Core.Hash
-- import Pact.Core.Names
-- import Pact.Core.Pacts.Types
-- import Pact.Core.ChainData
-- import Pact.Core.Errors
-- import Pact.Core.Gas



-- Note: The following functions
-- when placed in this file are causing GHC 9.6.2 to bork with the following error:
-- <no location info>: error:
--     panic! (the 'impossible' happened)
--   GHC version 9.6.2:
-- 	lookupIdSubst
--   $dMonadEvalEnv_aO5i
--   InScope {b_aNXG i_aNXH m_aNXI s_aNXJ a_aNXK $d(%,,,%)_aNXL
--            mkBuiltinFn cfFQN fromPactValue setEvalState overEvalState
--            useEvalState usesEvalState viewEvalEnv}
--   Call stack:
--       CallStack (from HasCallStack):
--         callStackDoc, called at compiler/GHC/Utils/Panic.hs:189:37 in ghc:GHC.Utils.Panic
--         pprPanic, called at compiler/GHC/Core/Subst.hs:197:17 in ghc:GHC.Core.Subst
--   CallStack (from HasCallStack):
--     panic, called at compiler/GHC/Utils/Error.hs:454:29 in ghc:GHC.Utils.Error
-- viewEvalEnv :: (MonadEval b i m) => Lens' (EvalEnv b i) s -> m s
-- viewEvalEnv l = view l <$> readEnv

-- viewsEvalEnv :: (MonadEval b i m) => Lens' (EvalEnv b i) s -> (s -> a) -> m a
-- viewsEvalEnv f l = views f l <$> readEnv

-- setEvalState :: (MonadEval b i m) => Traversal' (EvalState b i) s -> s -> m ()
-- setEvalState l s = modifyEvalState (set l s)

-- (.==) :: (MonadEval b i m) => Traversal' (EvalState b i) s -> s -> m ()
-- l .== s = modifyEvalState (set l s)

-- overEvalState :: (MonadEval b i m) => Lens' (EvalState b i) s -> (s -> s) -> m ()
-- overEvalState l f = modifyCEKState (over l f)

-- (%==) :: (MonadEval b i m) => Traversal' (EvalState b i) s -> (s -> s) -> m ()
-- l %== f = modifyEvalState (over l f)

-- infix 4 %==, .==

-- useEvalState :: (MonadEval b i m) => Lens' (EvalState b i) s -> m s
-- useEvalState l = view l <$> getEvalState

-- usesEvalState :: (MonadEval b i m) => Lens' (EvalState b i) s -> (s -> s') -> m s'
-- usesEvalState l f = views l f <$> getEvalState
