module Micron.Example.Empty (Empty (..)) where

import Data.Aeson
  ( FromJSON (omittedField, parseJSON),
    ToJSON (omitField, toEncoding, toJSON),
    Value (Null),
  )
import Data.Aeson.Encoding (null_)

data Empty = Empty

instance FromJSON Empty where
  parseJSON _ = fail "Field must not be present"
  omittedField = Just Empty

instance ToJSON Empty where
  toEncoding _ = null_
  toJSON _ = Null
  omitField _ = True