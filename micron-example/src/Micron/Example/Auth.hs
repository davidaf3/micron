{-# LANGUAGE OverloadedStrings #-}

module Micron.Example.Auth (authenticated) where

import Data.ByteString.Char8 qualified as BC
import Data.Functor ((<&>))
import Data.List (find)
import Data.Maybe (isJust)
import Data.Text.Encoding (decodeUtf8)
import Micron
  ( BaseErrorType (..),
    Error (..),
    Middleware,
    Request (..),
    errorRes,
  )
import Micron.Example.Config (RequestExtra (RequestExtra))
import Micron.Example.Resource.UserToken.Service (getUserByToken)

authenticated :: Middleware RequestExtra
authenticated h req = do
  let auth = find ((== "Authorization") . fst) $ headers req
  case auth >>= BC.stripPrefix "Bearer " . snd <&> decodeUtf8 of
    Nothing ->
      return $
        errorRes (Error Unauthorized "Invalid or missing Authorization header") req
    (Just token) -> do
      maybeUser <- getUserByToken token
      if isJust maybeUser
        then h req {extraData = RequestExtra maybeUser}
        else return $ errorRes (Error Forbidden "Forbidden") req
