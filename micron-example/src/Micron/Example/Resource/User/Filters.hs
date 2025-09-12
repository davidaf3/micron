{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveAnyClass #-}

module Micron.Example.Resource.User.Filters (UserFilters (..), applyFilters) where

import Data.Text qualified as T
import Database.Selda
  ( Query,
    Row,
    Set (isIn),
    literal,
    restrict,
    (!),
    (.==)
  )
import GHC.Generics (Generic)
import Micron (FromQueryString)
import Micron.Example.Resource.User.Model (User)

data UserFilters = UserFilters
  { name :: Maybe T.Text,
    names :: Maybe [T.Text]
  }
  deriving (Generic, FromQueryString)

applyFilters :: UserFilters -> Row a User -> Query a ()
applyFilters filters user = do
  mapM_ (\x -> restrict (user ! #userName .== literal x)) $ name filters
  mapM_ (\x -> restrict (user ! #userName `isIn` map literal x)) $ names filters
