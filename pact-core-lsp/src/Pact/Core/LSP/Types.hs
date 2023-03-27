-- |

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Pact.Core.LSP.Types where

import Language.LSP.Server
import Data.Text (Text)
import Control.Monad.State
import Control.Monad.Except
import Control.Lens
import GHC.Generics
import Language.LSP.Types (Uri)
import Data.Aeson (FromJSON, ToJSON)

data ServerConfig
  = ServerConfig
    { _scPactExec :: String
    }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)



data ServerState
  = ServerState
  {
  }
makeLenses ''ServerState

data HandlerError
  = UnknownError Text
  | NoVirtualFile Uri
  | NotCached Uri
  | PositionError
  deriving Show


newtype HandlerM a
  = HandlerM { unHandlerM :: ExceptT HandlerError (StateT ServerState (LspT ServerConfig IO)) a }
  deriving newtype (Functor,Applicative, Monad, MonadIO, MonadError HandlerError, MonadState ServerState)

runHandlerM :: HandlerM a -> ServerState -> LspT ServerConfig IO (Either HandlerError a, ServerState)
runHandlerM (HandlerM handler) = runStateT (runExceptT handler)
