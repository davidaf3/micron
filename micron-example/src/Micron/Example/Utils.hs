module Micron.Example.Utils (FromJSONVia (..), ToJSONVia (..), AppM, MonadApp (..)) where

import Control.Monad.Except (ExceptT)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Micron (Error, FromRequestBody (fromAppJson), ToResponseContent (toAppJson))

newtype ToJSONVia a = ToJSONVia a

instance (ToJSON a) => ToResponseContent (ToJSONVia a) where
  toAppJson (ToJSONVia x) = Just $ encode x

newtype FromJSONVia a = FromJSONVia a

instance (FromJSON a) => FromRequestBody (FromJSONVia a) where
  fromAppJson x = FromJSONVia <$> eitherDecode x

type AppM = ExceptT Error IO

class (Monad m) => MonadApp m where
  liftAppM :: AppM a -> m a

instance MonadApp AppM where
  liftAppM = id
