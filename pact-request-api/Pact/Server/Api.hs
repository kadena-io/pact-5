{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Pact.Server.Api
( -- * The Pact public REST Api
  type ApiV1Api
, apiV1Api
, pactServerApi
, apiV1Client
  -- ** Send
, ApiSend
, sendClient
  -- ** Listen
, ApiListen
, listenClient
  -- ** Poll
, ApiPoll
, pollClient
  -- ** Local
, ApiLocal
, localClient
  -- ** Version
, ApiVersion
, versionClient
) where


import Data.Proxy
import Data.Text (Text)

import Servant
import Servant.Client


-- -------------------------------------------------------------------- --
-- Public Pact REST Api

type ApiV1Api
    = "api"
    :> "v1"
    :>
      ( ApiSend
      :<|> ApiPoll
      :<|> ApiListen
      :<|> ApiLocal
      )

apiV1Api :: Proxy ApiV1Api
apiV1Api = Proxy

pactServerApi :: Proxy PactServerApi
pactServerApi = Proxy

apiV1Client :: forall m. RunClient m => ApiV1Client m
apiV1Client = ApiV1Client send poll listen local
  where
    send :<|> poll :<|> listen :<|> local :<|> _version =
      clientIn pactServerApi (Proxy :: Proxy m)

-- -------------------------------------------------------------------- --
-- POST /api/v1/send

type ApiSend = "send"
  :> ReqBody '[PactJson] SubmitBatch
  :> Post '[PactJson] RequestKeys

sendClient :: SubmitBatch -> ClientM RequestKeys
sendClient = v1Send apiV1Client

-- -------------------------------------------------------------------- --
-- POST /api/v1/poll

type ApiPoll = "poll"
  :> ReqBody '[PactJson] Poll
  :> Post '[PactJson] PollResponses

pollClient :: Poll -> ClientM PollResponses
pollClient = v1Poll apiV1Client

-- -------------------------------------------------------------------- --
-- POST /api/v1/listen

type ApiListen = "listen"
  :> ReqBody '[PactJson] ListenerRequest
  :> Post '[PactJson] ListenResponse

listenClient :: ListenerRequest -> ClientM ListenResponse
listenClient = v1Listen apiV1Client

-- -------------------------------------------------------------------- --
-- POST /api/v1/local

type ApiLocal = "local"
  :> ReqBody '[PactJson] (Command Text)
  :> Post '[PactJson] (CommandResult Hash)

localClient :: Command Text -> ClientM (CommandResult Hash)
localClient = v1Local apiV1Client

-- -------------------------------------------------------------------- --
-- GET /api/v1/version

type ApiVersion = "version"
  :> Get '[PlainText] Text

versionClient :: ClientM Text
versionClient = client (Proxy @ApiVersion)
