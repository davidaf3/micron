{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Micron.Example.Resource.User.Model
  ( User (..),
    UserView (..),
    LoginData (..),
    users,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T
import Database.Selda (Attr ((:-)), SqlRow, Table, primary, table, unique)
import GHC.Generics (Generic)
import Micron (FromRequestBody (..), ToResponseContent (..))
import Micron.Example.Utils (FromJSONVia (FromJSONVia), ToJSONVia (ToJSONVia))

data User = User
  { userId :: T.Text,
    password :: T.Text,
    userName :: T.Text
  }
  deriving (Generic, Show, SqlRow)

users :: Table User
users = table "user" [#userId :- primary, #userName :- unique]

data UserView = UserView {viewUserId :: T.Text, viewUserName :: T.Text}
  deriving (Generic, Show, ToJSON)
  deriving (ToResponseContent) via (ToJSONVia UserView)

deriving via (ToJSONVia [UserView]) instance (ToResponseContent [UserView])

data LoginData = LoginData {loginPassword :: T.Text, loginUserName :: T.Text}
  deriving (Generic, Show, FromJSON)
  deriving (FromRequestBody) via (FromJSONVia LoginData)
