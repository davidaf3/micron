{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Micron.Example.Resource.User.Model
  ( User' (..),
    User,
    UserWoID,
    UserWoIDWoPass,
    users,
    withId,
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
import Database.Selda (Attr ((:-)), SqlRow, Table, primary, table)
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

type UserWoID = User' Empty T.Text

type UserWoIDWoPass = User' Empty Empty

users :: Table User
users = table "user" [#userId :- primary]

withId :: User' a b -> T.Text -> User' T.Text b
withId u i = u {userId = i}

woPass :: User' a b -> User' a Empty
woPass u = u {password = Empty}

instance SqlRow User

instance (ToJSON a, ToJSON b) => ToJSON (User' a b) where
  toEncoding = genericToEncoding defaultOptions {omitNothingFields = True}

instance ToResponseContent User where
  toAppJson = Just . encode . woPass

instance ToResponseContent [User] where
  toAppJson = Just . encode . map woPass

instance FromJSON UserWoID

instance FromRequestBody UserWoID where
  fromAppJson = decode

instance FromJSON UserWoIDWoPass

instance FromRequestBody UserWoIDWoPass where
  fromAppJson = decode
