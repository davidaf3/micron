{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Micron.Extractor
  ( (~>),
    (~.),
    mkExtractor,
    mkInfallibleExtractor,
    param,
    query,
    body,
  )
where

import Control.Monad.Except (throwError, MonadError)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Micron.Error (BaseErrorType (InvalidArgument), Error (Error))
import Micron.MIMEType (appJson)
import Micron.Request
  ( BaseRequest (..),
    FromQueryString (..),
    FromRequestBody (..),
    Parseable (..),
    Request (..),
  )
import Network.HTTP.Types (hContentType)
import Control.Monad (join)

infixl 2 ~>

(~>) :: (Monad m) => (h -> r -> m (m o)) -> h -> r -> m o
(~>) exs h req = join (exs h req)

infixl 9 ~.

(~.) :: (Monad m) => (h -> r -> m h') -> (h' -> r -> m h'') -> h -> r -> m h''
(~.) exA exB h req = exA h req >>= flip exB req

mkExtractor :: (MonadError Error m) => (r -> Either e a) -> (e -> Error) -> (a -> h') -> r -> m h'
mkExtractor f toErr h req = case f req of
  Right x -> return $ h x
  Left err -> throwError $ toErr err

mkInfallibleExtractor :: (Monad m) => (r -> a) -> (a -> h') -> r -> m h'
mkInfallibleExtractor f h req = return $ h (f req)

extractParam :: (Request r, Parseable a) => String -> (r -> Either String a)
extractParam name req =
  let arg = Map.lookup (T.pack name) $ params $ getBaseRequest req
   in parseText $ fromMaybe T.empty arg

class Param h h' | h -> h' where
  param :: (MonadError Error m, Request r) => String -> h -> r -> m h'

instance {-# OVERLAPPABLE #-} (Parseable a) => Param (a -> h') h' where
  param name = mkExtractor (extractParam name) toErr
    where
      toErr err = Error InvalidArgument $ "Invalid " ++ name ++ ": " ++ err

instance {-# OVERLAPPING #-} (Parseable a) => Param (Either String a -> h') h' where
  param name = mkInfallibleExtractor $ extractParam name

extractQuery :: (Request r, FromQueryString a) => r -> Either (Map String String) a
extractQuery = fromQueryString . queryString . getBaseRequest

class Query h h' | h -> h' where
  query :: (MonadError Error m, Request r) => h -> r -> m h'

instance {-# OVERLAPPABLE #-} (FromQueryString a) => Query (a -> h') h' where
  query = mkExtractor extractQuery toErr
    where
      toErr err = Error InvalidArgument $ "Invalid query string: " ++ show err

instance {-# OVERLAPPING #-} (FromQueryString a) => Query (Either (Map String String) a -> h') h' where
  query = mkInfallibleExtractor extractQuery

extractBody :: (Request r, FromRequestBody a) => r -> Either String a
extractBody req =
  let reqBody = requestBody $ getBaseRequest req
   in case filter ((== hContentType) . fst) $ headers $ getBaseRequest req of
        [] -> fromAppJson reqBody
        ((_, accept) : _)
          | accept == appJson -> fromAppJson reqBody
          | otherwise -> fromAppJson reqBody

class Body h h' | h -> h' where
  body :: (MonadError Error m, Request r) => h -> r -> m h'

instance {-# OVERLAPPABLE #-} (FromRequestBody a) => Body (a -> h') h' where
  body = mkExtractor extractBody toErr
    where
      toErr err = Error InvalidArgument $ "Invalid request body: " ++ err

instance {-# OVERLAPPING #-} (FromRequestBody a) => Body (Either String a -> h') h' where
  body = mkInfallibleExtractor extractBody
