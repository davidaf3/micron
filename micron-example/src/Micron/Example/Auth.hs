{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Micron.Example.Auth (user, authenticated) where

import Data.ByteString.Char8 qualified as BC
import Data.Functor ((<&>))
import Data.List (find)
import Data.Text.Encoding (decodeUtf8)
import Micron
  ( BaseErrorType (..),
    Error (..),
    Middleware,
    Request (headers),
    mkExtractor,
  )
import Micron.Example.Db (DbAccess)
import Micron.Example.Resource.User.Model (User)
import Micron.Example.Resource.UserToken.Service (getUserByToken)
import Polysemy (Member, Members, Sem)
import Polysemy.Error (throw)
import Polysemy.Error qualified as PE (Error)
import Polysemy.Reader (Reader, ask, runReader)

user :: (Member (Reader User) r) => (User -> h) -> req -> Sem r h
user = mkExtractor (const ask)

authenticated :: (Members '[DbAccess, PE.Error Error] r) => Middleware (Sem (Reader User ': r)) (Sem r)
authenticated h req = do
  let auth = find ((== "Authorization") . fst) $ headers req
  case auth >>= BC.stripPrefix "Bearer " . snd <&> decodeUtf8 of
    Nothing ->
      throw (Error Unauthorized "Invalid or missing Authorization header")
    (Just token) ->
      getUserByToken token >>= \case
        Just u -> runReader u (h req)
        Nothing -> throw (Error Forbidden "Forbidden")
