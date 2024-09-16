module Micron.Example.Config (RequestExtra (..), defaultRequestExtra) where

import Micron.Example.Resource.User.Model (User)

newtype RequestExtra = RequestExtra
  { user :: Maybe User
  }

defaultRequestExtra :: RequestExtra
defaultRequestExtra = RequestExtra Nothing
