{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib.Config where

import GHC.Generics
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- | Application settings configuration
data Settings = Settings
  { settingsPort :: Int
  , settingsHost :: Text
  , settingsEnvironment :: Text
  , settingsAuth0Domain :: Text
  , settingsAuth0Audience :: Text
  , settingsAuth0ClientId :: Text
  , settingsAuth0ClientSecret :: Text
  , settingsMindsDbUrl :: Text
  , settingsCorsOrigins :: [Text]
  } deriving (Show, Generic)

-- | Default settings
defaultSettings :: Settings
defaultSettings = Settings
  { settingsPort = 7030
  , settingsHost = "0.0.0.0"
  , settingsEnvironment = "development"
  , settingsAuth0Domain = ""
  , settingsAuth0Audience = "https://api.nexpo.dev"
  , settingsAuth0ClientId = ""
  , settingsAuth0ClientSecret = ""
  , settingsMindsDbUrl = "http://localhost:4040"
  , settingsCorsOrigins = ["http://localhost:3000"]
  }

-- | Load settings from environment variables with fallback to defaults
loadSettings :: IO Settings
loadSettings = do
  port <- maybe (settingsPort defaultSettings) id <$> 
          (readMaybe <$>) <$> lookupEnv "HASKELL_API_PORT"
  host <- maybe (settingsHost defaultSettings) T.pack <$> lookupEnv "HOST"
  env <- maybe (settingsEnvironment defaultSettings) T.pack <$> lookupEnv "NODE_ENV"
  
  auth0Domain <- maybe (settingsAuth0Domain defaultSettings) T.pack <$> lookupEnv "AUTH0_DOMAIN"
  auth0Audience <- maybe (settingsAuth0Audience defaultSettings) T.pack <$> lookupEnv "AUTH0_AUDIENCE"
  auth0ClientId <- maybe (settingsAuth0ClientId defaultSettings) T.pack <$> lookupEnv "AUTH0_CLIENT_ID"
  auth0ClientSecret <- maybe (settingsAuth0ClientSecret defaultSettings) T.pack <$> lookupEnv "AUTH0_CLIENT_SECRET"
  
  mindsDbUrl <- maybe (settingsMindsDbUrl defaultSettings) T.pack <$> lookupEnv "MINDSDB_URL"
  
  corsOriginsStr <- lookupEnv "CORS_ORIGINS"
  let corsOrigins = maybe (settingsCorsOrigins defaultSettings) 
                          (map T.strip . T.splitOn "," . T.pack) corsOriginsStr
  
  return Settings
    { settingsPort = port
    , settingsHost = host
    , settingsEnvironment = env
    , settingsAuth0Domain = auth0Domain
    , settingsAuth0Audience = auth0Audience
    , settingsAuth0ClientId = auth0ClientId
    , settingsAuth0ClientSecret = auth0ClientSecret
    , settingsMindsDbUrl = mindsDbUrl
    , settingsCorsOrigins = corsOrigins
    }
