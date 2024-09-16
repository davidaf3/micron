{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Micron.Example.Resource.Post.Model (Post (..), PostInput (..), posts) where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
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
import Micron (FromRequestBody (..), ToResponseContent (..))
import Micron.Example.Resource.User.Model (users)

data Post = Post
  { postId :: T.Text,
    userId :: T.Text,
    content :: T.Text
  }
  deriving (Generic, Show)

posts :: Table Post
posts =
  table
    "post"
    [ #postId :- primary,
      #userId :- foreignKey users #userId
    ]

instance SqlRow Post

instance ToJSON Post

instance ToResponseContent Post where
  toAppJson = Just . encode

instance ToResponseContent [Post] where
  toAppJson = Just . encode

newtype PostInput = PostInput {content :: T.Text} deriving (Generic, Show)

instance FromJSON PostInput

instance FromRequestBody PostInput where
  fromAppJson = decode
