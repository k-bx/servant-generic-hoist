{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Reader
import Data.Text (Text)
import GHC.Generics
import Network.Wai (Request)
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Servant
import Servant.API.Generic ((:-), ToServantApi, genericApi)
import Servant.Server
import Servant.Server.Experimental.Auth
  ( AuthHandler
  , AuthServerData
  , mkAuthHandler
  )
import Servant.Server.Experimental.Auth (AuthHandler)
import Servant.Server.Generic (AsServerT, genericServerT)

data Env =
  Env

type AppM = ReaderT Env Servant.Handler

data API route = API
  { _index :: route :- Get '[ PlainText] Text
  , _ping :: route :- "api" :> "ping" :> Get '[ PlainText] Text
  } deriving (Generic)

api :: Proxy (ToServantApi API)
api = genericApi (Proxy :: Proxy API)

server :: API (AsServerT AppM)
server = API {_index = pure "hey", _ping = pure "pong"}

nt :: Env -> AppM a -> Servant.Handler a
nt s x = runReaderT x s

type UserId = Int

auth :: Context (AuthHandler Request UserId ': '[])
auth = (mkAuthHandler (const (pure 1))) :. EmptyContext

main :: IO ()
main = do
  let env = Env
  Warp.run
    8000
    (serveWithContext
       (Proxy :: Proxy (ToServantApi API))
       auth
       (hoistServerWithContext
          (Proxy :: Proxy (ToServantApi API))
          (Proxy :: Proxy '[ UserId])
          (nt env)
          (genericServerT server)))
