{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Reader
import Data.Text (Text)
import GHC.Generics
import Network.Wai (Request)
import qualified Network.Wai.Handler.Warp as W
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.Warp.Internal as W
import Servant
import Servant
import Servant.Generic
import Servant.Server
import Servant.Server.Experimental.Auth
  ( AuthHandler
  , AuthServerData
  , mkAuthHandler
  )
import Servant.Server.Experimental.Auth (AuthHandler)
import Servant.Utils.Enter

data Env =
  Env

type AppM = ReaderT Env Servant.Handler

type API = ToServant (AdServerAPI AsApi) :<|> ToServant (StaticAPI AsApi)

data AdServerAPI route = AdServerAPI
  { _foo :: route :- "foo" :> Get '[ PlainText] Text
  } deriving (Generic)

data StaticAPI route = StaticAPI
  { static :: route :- "static" :> Raw
  } deriving (Generic)

api :: Proxy API
api = Proxy

server :: AdServerAPI (AsServerT AppM)
server = AdServerAPI {_foo = pure "foo"}

nt :: Env -> AppM a -> Servant.Handler a
nt s x = runReaderT x s

type UserId = Int

auth :: Context (AuthHandler Request UserId ': '[])
auth = (mkAuthHandler (const (pure 1))) :. EmptyContext

main :: IO ()
main = do
  let env = Env
  let application = serveWithContext api auth (completeServer env)
  let settings = W.defaultSettings
  W.runSettings settings application

srvStatic :: StaticAPI AsServer
srvStatic = StaticAPI {static = serveDirectoryWebApp "."}

completeServer :: Env -> Server API
completeServer env = handlerServer env :<|> toServant srvStatic

handlerServer :: Env -> ToServant (AdServerAPI AsServer)
handlerServer env = enter appToHandler (toServant server)
  where
    appToHandler :: AppM :~> Handler
    appToHandler = runReaderTNat env
