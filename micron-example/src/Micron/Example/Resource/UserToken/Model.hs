{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Micron.Example.Resource.UserToken.Model (UserToken (..), userTokens) where

import Data.Aeson (ToJSON, encode)
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
import Micron (ToResponseContent (..))
import Micron.Example.Resource.User.Model (users)

data UserToken = UserToken
  { token :: T.Text,
    userId :: T.Text
  }
  deriving (Generic, Show)

userTokens :: Table UserToken
userTokens =
  table
    "user_token"
    [ #token :- primary,
      #userId :- foreignKey users #userId
    ]

instance SqlRow UserToken

instance ToJSON UserToken

instance ToResponseContent UserToken where
  toAppJson = Just . encode
