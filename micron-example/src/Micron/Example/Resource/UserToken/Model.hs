{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Micron.Example.Resource.UserToken.Model (UserToken (..), userTokens) where

import Data.Aeson (ToJSON)
import Data.Text qualified as T
import Database.Selda
  ( Attr ((:-)),
    ForeignKey (foreignKey),
    SqlRow,
    Table,
    primary,
    table,
  )
import GHC.Generics (Generic)
import Micron (ToResponseContent)
import Micron.Example.Resource.User.Model (users)
import Micron.Example.Utils (ToJSONVia (ToJSONVia))

data UserToken = UserToken
  { token :: T.Text,
    userId :: T.Text
  }
  deriving (Generic, Show, SqlRow, ToJSON)
  deriving (ToResponseContent) via (ToJSONVia UserToken)

userTokens :: Table UserToken
userTokens =
  table
    "user_token"
    [ #token :- primary,
      #userId :- foreignKey users #userId
    ]
