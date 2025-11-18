module Micron.Example.Utils (FromJSONVia (..), ToJSONVia (..)) where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Micron (FromRequestBody (fromAppJson), ToResponseContent (toAppJson))

newtype ToJSONVia a = ToJSONVia a

instance (ToJSON a) => ToResponseContent (ToJSONVia a) where
  toAppJson (ToJSONVia x) = Just $ encode x

newtype FromJSONVia a = FromJSONVia a

instance (FromJSON a) => FromRequestBody (FromJSONVia a) where
  fromAppJson x = FromJSONVia <$> eitherDecode x
