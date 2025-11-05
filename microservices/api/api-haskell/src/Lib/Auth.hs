{-# LANGUAGE OverloadedStrings #-}

module Lib.Auth where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Network.HTTP.Simple
import Control.Monad.IO.Class (liftIO, MonadIO)
import Lib.Types
import Lib.Config

-- | JWT token verification
-- This is a simplified implementation - in production, use a proper JWT library
verifyToken :: Settings -> Text -> IO (Maybe UserContext)
verifyToken settings token = do
  -- In a real implementation, you would:
  -- 1. Get JWKS from Auth0
  -- 2. Verify the token signature
  -- 3. Check expiration and claims
  -- 4. Extract user information
  
  -- For now, return a mock user context if token is not empty
  if T.null token
    then return Nothing
    else return $ Just $ UserContext
      { contextSub = "auth0|mock_user_id"
      , contextEmail = Just "user@example.com"
      , contextName = Just "Mock User"
      , contextPicture = Just "https://example.com/avatar.jpg"
      , contextEmailVerified = True
      }

-- | Extract bearer token from authorization header
extractBearerToken :: Text -> Maybe Text
extractBearerToken authHeader =
  if "Bearer " `T.isPrefixOf` authHeader
    then Just $ T.drop 7 authHeader
    else Nothing

-- | Get JWKS from Auth0 (mock implementation)
getJwks :: Settings -> IO (Maybe Value)
getJwks settings = do
  let jwksUrl = "https://" <> settingsAuth0Domain settings <> "/.well-known/jwks.json"
  if T.null (settingsAuth0Domain settings)
    then return Nothing
    else do
      -- In production, make actual HTTP request to get JWKS
      -- For now, return Nothing to indicate JWKS not available
      return Nothing

-- | Validate JWT token against JWKS (mock implementation)
validateJwt :: Value -> Text -> IO Bool
validateJwt _jwks _token = do
  -- In production, use a proper JWT library like jose to validate
  -- For now, always return True for non-empty tokens
  return True
