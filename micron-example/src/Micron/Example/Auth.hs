{-# LANGUAGE OverloadedStrings #-}

module Micron.Example.Auth (authenticated) where

import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Char8 qualified as BC
import Data.Functor ((<&>))
import Data.List (find)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Micron
  ( BaseErrorType (..),
    Error (..),
    Middleware,
    Request (headers),
    errorRes,
  )
import Micron.Example.Crypto (checkPassword)
import Micron.Example.Resource.User.Model (User' (password))
import Micron.Example.Resource.User.Service (getUser)

parseAuth :: BC.ByteString -> Maybe (T.Text, T.Text)
parseAuth header = case BC.stripPrefix "Basic " header <&> Base64.decode of
  Just (Right credentials) -> case BC.split ':' credentials of
    [uId, pass] -> Just (decodeUtf8 uId, decodeUtf8 pass)
    _ -> Nothing
  _ -> Nothing

authenticated :: Middleware
authenticated h req = do
  let auth = find ((== "Authorization") . fst) $ headers req
  case auth >>= parseAuth . snd of
    Nothing ->
      return $
        errorRes (Error Unauthorized "Invalid or missing Authorization header") req
    Just (uId, pass) -> do
      eitherUser <- getUser $ Right uId
      if either (const False) (checkPassword pass . password) eitherUser
        then h req
        else return $ errorRes (Error Forbidden "Forbidden") req
