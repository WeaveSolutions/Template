{-# LANGUAGE OverloadedStrings #-}

module Lib.Handlers where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Aeson
import Control.Monad.IO.Class (liftIO)
import Servant
import System.Random (randomRIO)
import Lib.Types
import Lib.Config
import Lib.Auth

-- | Root endpoint - health check
rootHandler :: Settings -> Handler HealthResponse
rootHandler settings = liftIO $ do
  now <- getCurrentTime
  return HealthResponse
    { healthStatus = "ok"
    , healthService = "nexpo-haskell-api"
    , healthTimestamp = T.pack $ show now
    , healthVersion = "1.0.0"
    , healthEnvironment = settingsEnvironment settings
    }

-- | Detailed health check endpoint
healthHandler :: Settings -> Handler HealthResponse
healthHandler settings = liftIO $ do
  now <- getCurrentTime
  return HealthResponse
    { healthStatus = "healthy"
    , healthService = "nexpo-haskell-api"
    , healthTimestamp = T.pack $ show now
    , healthVersion = "1.0.0"
    , healthEnvironment = settingsEnvironment settings
    }

-- | Get current user profile
userProfileHandler :: Settings -> Maybe Text -> Handler UserProfile
userProfileHandler settings Nothing = throwError err401
userProfileHandler settings (Just authHeader) = do
  case extractBearerToken authHeader of
    Nothing -> throwError err401
    Just token -> do
      maybeUser <- liftIO $ verifyToken settings token
      case maybeUser of
        Nothing -> throwError err401
        Just user -> return UserProfile
          { userSub = contextSub user
          , userEmail = contextEmail user
          , userName = contextName user
          , userPicture = contextPicture user
          , userEmailVerified = contextEmailVerified user
          , userMetadata = Map.empty
          }

-- | Get user's linked social accounts
userAccountsHandler :: Settings -> Maybe Text -> Handler [LinkedAccount]
userAccountsHandler settings Nothing = throwError err401
userAccountsHandler settings (Just authHeader) = do
  case extractBearerToken authHeader of
    Nothing -> throwError err401
    Just token -> do
      maybeUser <- liftIO $ verifyToken settings token
      case maybeUser of
        Nothing -> throwError err401
        Just _user -> liftIO $ do
          now <- getCurrentTime
          let pastTime = addUTCTime (-2592000) now -- 30 days ago
          return 
            [ LinkedAccount
                { linkedProvider = "google-oauth2"
                , linkedUserId = "google-oauth2|123456789"
                , linkedConnection = "google-oauth2"
                , linkedIsSocial = True
                , linkedAt = T.pack $ show pastTime
                }
            ]

-- | Exchange credentials for access token
tokenHandler :: Settings -> TokenRequest -> Handler TokenResponse
tokenHandler _settings tokenReq = do
  if tokenReqGrantType tokenReq /= "client_credentials"
    then throwError err400 { errBody = "Only client_credentials grant type is supported" }
    else liftIO $ do
      -- Generate a mock token
      randomSuffix <- randomRIO (100000000000000 :: Integer, 999999999999999)
      let mockToken = "mock_access_token_" <> T.pack (show randomSuffix)
      return TokenResponse
        { tokenAccess = mockToken
        , tokenType = "Bearer"
        , tokenExpiresIn = 3600
        , tokenScope = tokenReqScope tokenReq
        }

-- | Execute query through MindsDB gateway
queryHandler :: Settings -> Maybe Text -> QueryRequest -> Handler QueryResponse
queryHandler settings Nothing _req = throwError err401
queryHandler settings (Just authHeader) req = do
  case extractBearerToken authHeader of
    Nothing -> throwError err401
    Just token -> do
      maybeUser <- liftIO $ verifyToken settings token
      case maybeUser of
        Nothing -> throwError err401
        Just _user -> liftIO $ do
          startTime <- getCurrentTime
          -- Mock MindsDB query execution
          endTime <- getCurrentTime
          let executionTime = round $ diffUTCTime endTime startTime * 1000
          now <- getCurrentTime
          return QueryResponse
            { responseQuery = queryText req
            , responseResults = Just 
                [ object 
                    [ "message" .= ("Mock response" :: Text)
                    , "query" .= queryText req
                    , "timestamp" .= show now
                    ]
                ]
            , responseExecutionTimeMs = executionTime
            , responseStatus = "success"
            }
