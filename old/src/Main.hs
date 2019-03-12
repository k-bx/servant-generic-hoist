{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Data.Swagger

import Control.Monad.Trans.Reader
import Data.Swagger
import Data.Swagger.Declare (Declare)
import Data.Swagger.Declare (Declare)
import Data.Swagger.Internal.Schema (named, plain, timeSchema, unnamed)
import Data.Swagger.Internal.Schema (named, plain, timeSchema, unnamed)
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Client (Manager)
import qualified Network.HTTP.Client as HTTPClient
import Network.Wai (Request, pathInfo, responseLBS)
import qualified Network.Wai.Handler.Warp as W
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.Warp.Internal as W
import Network.Wreq (withManager)
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
import Servant.Swagger
import Servant.Swagger.UI
import Servant.Utils.Enter
import Wai.Routes (mkRoute)
import qualified Wai.Routes as WaiRoutes

data Env =
  Env

type AppM = ReaderT Env Servant.Handler

type API
   = ToServant (AdServerAPI AsApi) :<|> ToServant (StaticAPI AsApi) :<|> SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> Raw

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
  mgr <-
    HTTPClient.newManager $
    HTTPClient.defaultManagerSettings {HTTPClient.managerConnCount = 30}
  let application = serveWithContext api auth (completeServer env mgr)
  let settings = W.defaultSettings
  W.runSettings settings application

srvStatic :: StaticAPI AsServer
srvStatic = StaticAPI {static = serveDirectoryWebApp "."}

completeServer :: Env -> Manager -> Server API
completeServer env mgr =
  handlerServer env :<|> toServant srvStatic :<|>
  swaggerSchemaUIServer swaggerDoc :<|>
  Tagged (legacy mgr)

swaggerDoc :: Swagger
swaggerDoc = toSwagger publicApi

publicApi :: Proxy (ToServant (AdServerAPI AsApi))
publicApi = Proxy

handlerServer :: Env -> ToServant (AdServerAPI AsServer)
handlerServer env = enter appToHandler (toServant server)
  where
    appToHandler :: AppM :~> Handler
    appToHandler = runReaderTNat env

data LegacyRoute =
  LegacyRoute Manager

legacy :: Manager -> Application
legacy mgr =
  WaiRoutes.waiApp $ do
    serveFiles
    WaiRoutes.route $ LegacyRoute mgr
    WaiRoutes.catchall urlNotFoundHandler

urlNotFoundHandler :: Application
urlNotFoundHandler rq respond = do
  respond . responseLBS WaiRoutes.status404 [] $ "Not Found"

serveFiles :: WaiRoutes.RouteM ()
serveFiles =
  WaiRoutes.handler . WaiRoutes.runHandlerM $ do
    p <- pathInfo <$> WaiRoutes.request
    case lookup p paths of
      Just fp -> WaiRoutes.file fp
      Nothing -> WaiRoutes.next
  where
    paths = fileMappings
    fileMappings :: [([Text], FilePath)]
    fileMappings = [(["foo", "bar.js"], "foo/bar.js")]

mkRoute
  "LegacyRoute"
  [WaiRoutes.parseRoutes|
/v1/tacos  TacosR  GET
|]

type LegacyHandler m = WaiRoutes.HandlerS LegacyRoute m

emptyLogJS :: LegacyHandler m
emptyLogJS =
  WaiRoutes.runHandlerM $ WaiRoutes.asContent "application/javascript" ""

getTacosR :: LegacyHandler m
getTacosR = emptyLogJS
