{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Micron.Example.Auth (user, authenticated) where

import Data.ByteString.Char8 qualified as BC
import Data.Functor ((<&>))
import Data.List (find)
import Data.Text.Encoding (decodeUtf8)
import Micron
  ( BaseErrorType (..),
    BaseRequest (headers),
    Error (..),
    Middleware,
    Request (..),
    errorRes,
    mkInfallibleExtractor,
  )
import Micron.Example.Resource.User.Model (User)
import Micron.Example.Resource.UserToken.Service (getUserByToken)

data AuthenticatedRequest r = AuthenticatedRequest
  { authenticatedUser :: User,
    inner :: r
  }

instance (Request r) => Request (AuthenticatedRequest r) where
  getBaseRequest = getBaseRequest . inner

user :: (User -> h) -> AuthenticatedRequest r -> Either Error h
user = mkInfallibleExtractor authenticatedUser

authenticated :: (Request r) => Middleware (AuthenticatedRequest r) r
authenticated h req = do
  let auth = find ((== "Authorization") . fst) $ headers $ getBaseRequest req
  case auth >>= BC.stripPrefix "Bearer " . snd <&> decodeUtf8 of
    Nothing ->
      return $ errorRes (Error Unauthorized "Invalid or missing Authorization header") req
    (Just token) ->
      getUserByToken token >>= \case
        Just u -> h $ AuthenticatedRequest u req
        Nothing -> return $ errorRes (Error Forbidden "Forbidden") req
