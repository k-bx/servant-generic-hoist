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
import Data.Swagger.Internal.Schema (named, plain, timeSchema, unnamed)
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Client (Manager)
import qualified Network.HTTP.Client as HTTPClient
import Network.Wai (Request, pathInfo, responseLBS)
import qualified Network.Wai.Handler.Warp as W
import qualified Network.Wai.Handler.Warp.Internal as W
import Network.Wreq (withManager)
import Servant
import Servant.API.Generic
import Servant.Server
import Servant.Server.Experimental.Auth
  ( AuthHandler
  , AuthServerData
  , mkAuthHandler
  )
import Servant.Server.Experimental.Auth (AuthHandler)
import Servant.Server.Generic
import Servant.Swagger
import Servant.Swagger.UI
import Wai.Routes (mkRoute)
import qualified Wai.Routes as WaiRoutes

data Env =
  Env

type AppM = ReaderT Env Servant.Handler

-- Full api type, combines few generic-based APIs, one swagger-ui schema and some "raw" stuff
type Swag = SwaggerSchemaUI "swagger-ui" "swagger.json"

type API
   = ToServantApi AdServerAPI :<|> ToServantApi StaticAPI :<|> SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> Raw

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

-- context
auth :: Context (AuthHandler Request UserId ': '[])
auth = (mkAuthHandler (const (pure 1))) :. EmptyContext

main :: IO ()
main = do
  let env = Env
  mgr <-
    HTTPClient.newManager $
    HTTPClient.defaultManagerSettings {HTTPClient.managerConnCount = 30}
  let hoisted :: ServerT API Servant.Handler
      hoisted =
        (hoistServerWithContext
           (Proxy :: Proxy API)
           (Proxy :: Proxy '[ UserId])
           (nt env)
           (completeServer env mgr))
  let application = serveWithContext api auth hoisted
  let settings = W.defaultSettings
  W.runSettings settings application

srvStatic :: StaticAPI (AsServerT AppM)
srvStatic = StaticAPI {static = serveDirectoryWebApp "."}

completeServer :: Env -> Manager -> ServerT API AppM
completeServer env mgr =
  let swag =
        hoistServer
          (Proxy :: Proxy Swag)
          ((\h -> ReaderT (\e -> h)) :: Handler x -> AppM x)
          (swaggerSchemaUIServer swaggerDoc)
   in genericServerT (handlerServer env) :<|> genericServerT srvStatic :<|> swag :<|>
      Tagged (legacy mgr)

swaggerDoc :: Swagger
swaggerDoc = toSwagger publicApi

publicApi :: Proxy (ToServantApi AdServerAPI)
publicApi = Proxy

-- enter is deprecated
handlerServer :: Env -> AdServerAPI (AsServerT AppM)
handlerServer env = server

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
