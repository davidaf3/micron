{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Micron.Example.Auth (user, authenticated) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT, runReaderT)
import Data.ByteString.Char8 qualified as BC
import Data.Functor ((<&>))
import Data.List (find)
import Data.Text.Encoding (decodeUtf8)
import Database.Selda (MonadMask)
import Micron
  ( BaseErrorType (..),
    Error (..),
    Middleware,
    Request (headers),
    errorRes,
    mkInfallibleExtractor,
  )
import Micron.Example.Resource.User.Model (User)
import Micron.Example.Resource.UserToken.Service (getUserByToken)

user :: (MonadReader User m) => (User -> h) -> r -> m h
user = mkInfallibleExtractor (const ask)

authenticated :: (MonadIO m, MonadMask m) => Middleware (ReaderT User m) m
authenticated h req = do
  let auth = find ((== "Authorization") . fst) $ headers req
  case auth >>= BC.stripPrefix "Bearer " . snd <&> decodeUtf8 of
    Nothing ->
      return $ errorRes (Error Unauthorized "Invalid or missing Authorization header") req
    (Just token) ->
      getUserByToken token >>= \case
        Just u -> runReaderT (h req) u
        Nothing -> return $ errorRes (Error Forbidden "Forbidden") req
