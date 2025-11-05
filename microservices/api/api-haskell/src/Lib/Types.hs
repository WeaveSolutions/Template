{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib.Types where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics
import Servant

-- | Health check response model
data HealthResponse = HealthResponse
  { healthStatus :: Text
  , healthService :: Text
  , healthTimestamp :: Text
  , healthVersion :: Text
  , healthEnvironment :: Text
  } deriving (Show, Generic)

instance ToJSON HealthResponse where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = \s -> case s of
        "healthStatus" -> "status"
        "healthService" -> "service"
        "healthTimestamp" -> "timestamp"
        "healthVersion" -> "version"
        "healthEnvironment" -> "environment"
        _ -> s
    }

-- | User profile model
data UserProfile = UserProfile
  { userSub :: Text
  , userEmail :: Maybe Text
  , userName :: Maybe Text
  , userPicture :: Maybe Text
  , userEmailVerified :: Bool
  , userMetadata :: Map Text Value
  } deriving (Show, Generic)

instance ToJSON UserProfile where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = \s -> case s of
        "userSub" -> "sub"
        "userEmail" -> "email"
        "userName" -> "name"
        "userPicture" -> "picture"
        "userEmailVerified" -> "email_verified"
        "userMetadata" -> "metadata"
        _ -> s
    }

-- | Error response model
data ErrorResponse = ErrorResponse
  { errorCode :: Text
  , errorMessage :: Text
  , errorDetails :: Maybe (Map Text Value)
  } deriving (Show, Generic)

instance ToJSON ErrorResponse where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = \s -> case s of
        "errorCode" -> "error"
        "errorMessage" -> "message"
        "errorDetails" -> "details"
        _ -> s
    }

-- | Token exchange response
data TokenResponse = TokenResponse
  { tokenAccess :: Text
  , tokenType :: Text
  , tokenExpiresIn :: Int
  , tokenScope :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON TokenResponse where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = \s -> case s of
        "tokenAccess" -> "access_token"
        "tokenType" -> "token_type"
        "tokenExpiresIn" -> "expires_in"
        "tokenScope" -> "scope"
        _ -> s
    }

-- | Linked social account
data LinkedAccount = LinkedAccount
  { linkedProvider :: Text
  , linkedUserId :: Text
  , linkedConnection :: Text
  , linkedIsSocial :: Bool
  , linkedAt :: Text
  } deriving (Show, Generic)

instance ToJSON LinkedAccount where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = \s -> case s of
        "linkedProvider" -> "provider"
        "linkedUserId" -> "user_id"
        "linkedConnection" -> "connection"
        "linkedIsSocial" -> "is_social"
        "linkedAt" -> "linked_at"
        _ -> s
    }

-- | MindsDB query request
data QueryRequest = QueryRequest
  { queryText :: Text
  } deriving (Show, Generic)

instance FromJSON QueryRequest where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = \s -> case s of
        "queryText" -> "query"
        _ -> s
    }

-- | MindsDB query response
data QueryResponse = QueryResponse
  { responseQuery :: Text
  , responseResults :: Maybe [Value]
  , responseExecutionTimeMs :: Int
  , responseStatus :: Text
  } deriving (Show, Generic)

instance ToJSON QueryResponse where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = \s -> case s of
        "responseQuery" -> "query"
        "responseResults" -> "results"
        "responseExecutionTimeMs" -> "execution_time_ms"
        "responseStatus" -> "status"
        _ -> s
    }

-- | Token exchange request
data TokenRequest = TokenRequest
  { tokenReqGrantType :: Text
  , tokenReqScope :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON TokenRequest where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = \s -> case s of
        "tokenReqGrantType" -> "grant_type"
        "tokenReqScope" -> "scope"
        _ -> s
    }

-- | User context from JWT
data UserContext = UserContext
  { contextSub :: Text
  , contextEmail :: Maybe Text
  , contextName :: Maybe Text
  , contextPicture :: Maybe Text
  , contextEmailVerified :: Bool
  } deriving (Show, Generic)
