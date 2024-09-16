{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Micron.Example.Resource.User.Model
  ( User' (..),
    User,
    LoginData,
    users,
  )
where

import Data.Aeson
  ( FromJSON,
    Options (omitNothingFields),
    ToJSON (toEncoding),
    decode,
    defaultOptions,
    encode,
    genericToEncoding,
  )
import Data.Text qualified as T
import Database.Selda (Attr ((:-)), SqlRow, Table, primary, table, unique)
import GHC.Generics (Generic)
import Micron (FromRequestBody (..), ToResponseContent (..))
import Micron.Example.Empty (Empty (..))

data User' a b = User
  { userId :: a,
    password :: b,
    userName :: T.Text
  }
  deriving (Generic, Show)

type User = User' T.Text T.Text

type LoginData = User' Empty T.Text

users :: Table User
users = table "user" [#userId :- primary, #userName :- unique]

instance SqlRow User

instance (ToJSON a, ToJSON b) => ToJSON (User' a b) where
  toEncoding = genericToEncoding defaultOptions {omitNothingFields = True}

instance ToResponseContent User where
  toAppJson u = Just $ encode $ u {password = Empty}

instance ToResponseContent [User] where
  toAppJson = Just . encode . map (\u -> u {password = Empty})

instance FromJSON LoginData

instance FromRequestBody LoginData where
  fromAppJson = decode
