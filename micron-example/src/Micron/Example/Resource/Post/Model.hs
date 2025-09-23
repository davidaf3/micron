{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Micron.Example.Resource.Post.Model (Post (..), PostInput (..), posts) where

import Data.Aeson (FromJSON, ToJSON)
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
import Micron (FromRequestBody, ToResponseContent)
import Micron.Example.Resource.User.Model (users)
import Micron.Example.Utils (FromJSONVia (FromJSONVia), ToJSONVia (ToJSONVia))

data Post = Post
  { postId :: T.Text,
    userId :: T.Text,
    content :: T.Text
  }
  deriving (Generic, Show, SqlRow, ToJSON)
  deriving (ToResponseContent) via (ToJSONVia Post)

deriving via (ToJSONVia [Post]) instance (ToResponseContent [Post])

posts :: Table Post
posts =
  table
    "post"
    [ #postId :- primary,
      #userId :- foreignKey users #userId
    ]

newtype PostInput = PostInput {content :: T.Text}
  deriving (Generic, Show, FromJSON)
  deriving (FromRequestBody) via (FromJSONVia PostInput)
