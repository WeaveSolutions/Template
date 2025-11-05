{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Server where

import Data.Text (Text)
import qualified Data.Text as T
import Servant
import Servant.Swagger
import Servant.Swagger.UI
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Control.Lens
import Data.Swagger
import Lib.Types
import Lib.Config
import Lib.Handlers

-- | API definition
type API = 
  -- Root health check
  Get '[JSON] HealthResponse
  :<|> "health" :> Get '[JSON] HealthResponse
  
  -- User endpoints
  :<|> "api" :> "user" :> "profile" 
    :> Header "Authorization" Text 
    :> Get '[JSON] UserProfile
  :<|> "api" :> "user" :> "accounts" 
    :> Header "Authorization" Text 
    :> Get '[JSON] [LinkedAccount]
  
  -- Auth endpoints
  :<|> "api" :> "token" 
    :> ReqBody '[JSON] TokenRequest 
    :> Post '[JSON] TokenResponse
  
  -- Query endpoints
  :<|> "api" :> "query" 
    :> Header "Authorization" Text 
    :> ReqBody '[JSON] QueryRequest 
    :> Post '[JSON] QueryResponse

-- | Swagger API with documentation
type SwaggerAPI = SwaggerSchemaUI "docs" "swagger.json" :<|> API

-- | API proxy
apiProxy :: Proxy API
apiProxy = Proxy

-- | Swagger API proxy
swaggerApiProxy :: Proxy SwaggerAPI
swaggerApiProxy = Proxy

-- | Create server implementation
server :: Settings -> Server API
server settings = 
  rootHandler settings
  :<|> healthHandler settings
  :<|> userProfileHandler settings
  :<|> userAccountsHandler settings
  :<|> tokenHandler settings
  :<|> queryHandler settings

-- | Create Swagger documentation
swaggerDoc :: Swagger
swaggerDoc = toSwagger apiProxy
  & info.title .~ "Nexpo Haskell API"
  & info.version .~ "1.0.0"
  & info.description ?~ "Basic configurable backend API for Nexpo - Haskell Implementation"
  & host ?~ Host "localhost" (Just 7030)

-- | Create server with Swagger UI
swaggerServer :: Settings -> Server SwaggerAPI
swaggerServer settings = swaggerSchemaUIServer swaggerDoc :<|> server settings

-- | Create WAI application
createApp :: Settings -> Application
createApp settings = 
  cors corsPolicy $ serve swaggerApiProxy (swaggerServer settings)
  where
    corsPolicy = const $ Just simpleCorsResourcePolicy
      { corsOrigins = 
          if settingsEnvironment settings == "development"
            then Nothing -- Allow all origins in development
            else Just (map T.encodeUtf8 (settingsCorsOrigins settings), False)
      , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS", "PATCH"]
      , corsRequestHeaders = 
          [ "Origin"
          , "X-Requested-With"
          , "Content-Type" 
          , "Accept"
          , "Authorization"
          , "X-Request-ID"
          , "X-Platform"
          , "X-Client-Version"
          ]
      }

-- | Run the server
runServer :: Settings -> IO ()
runServer settings = do
  let port = settingsPort settings
  putStrLn $ "🚀 Nexpo Haskell API starting on port " ++ show port
  putStrLn $ "📖 Swagger documentation available at http://localhost:" ++ show port ++ "/docs"
  putStrLn $ "🌍 Environment: " ++ T.unpack (settingsEnvironment settings)
  putStrLn $ "🔗 CORS origins: " ++ show (settingsCorsOrigins settings)
  run port (createApp settings)
