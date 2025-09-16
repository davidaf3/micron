{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

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

import Control.Arrow (ArrowChoice (left))
import Control.Monad (join)
import Control.Monad.Except (MonadError, liftEither)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Micron.Error (BaseErrorType (InvalidArgument), Error (Error))
import Micron.MIMEType (appJson)
import Micron.Request
  ( FromQueryString (..),
    FromRequestBody (..),
    Parseable (..),
    Request (..),
  )
import Network.HTTP.Types (hContentType)

infixl 2 ~>

(~>) :: (Monad m) => (h -> r -> m (m o)) -> h -> r -> m o
(~>) exs h req = join (exs h req)

infixl 9 ~.

(~.) :: (Monad m) => (h -> r -> m h') -> (h' -> r -> m h'') -> h -> r -> m h''
(~.) exA exB h req = exA h req >>= flip exB req

mkExtractor :: (MonadError Error m) => (r -> m a) -> (a -> h') -> r -> m h'
mkExtractor f h req = h <$> f req

mkInfallibleExtractor :: (Monad m) => (r -> m a) -> (a -> h') -> r -> m h'
mkInfallibleExtractor f h req = h <$> f req

extractParam :: (Parseable a) => String -> (Request -> Either String a)
extractParam name req =
  let arg = Map.lookup (T.pack name) $ params req
   in parseText $ fromMaybe T.empty arg

class Param h h' | h -> h' where
  param :: (MonadError Error m) => String -> h -> Request -> m h'

instance {-# OVERLAPPABLE #-} (Parseable a) => Param (a -> h') h' where
  param name = mkExtractor (liftEither . left toErr . extractParam name)
    where
      toErr err = Error InvalidArgument $ "Invalid " ++ name ++ ": " ++ err

instance {-# OVERLAPPING #-} (Parseable a) => Param (Either String a -> h') h' where
  param name = mkExtractor (return . extractParam name)

extractQuery :: (FromQueryString a) => Request -> Either (Map String String) a
extractQuery = fromQueryString . queryString

class Query h h' | h -> h' where
  query :: (MonadError Error m) => h -> Request -> m h'

instance {-# OVERLAPPABLE #-} (FromQueryString a) => Query (a -> h') h' where
  query = mkExtractor (liftEither . left toErr . extractQuery)
    where
      toErr err = Error InvalidArgument $ "Invalid query string: " ++ show err

instance {-# OVERLAPPING #-} (FromQueryString a) => Query (Either (Map String String) a -> h') h' where
  query = mkExtractor (return . extractQuery)

extractBody :: (FromRequestBody a) => Request -> Either String a
extractBody req =
  let reqBody = requestBody req
   in case filter ((== hContentType) . fst) $ headers req of
        [] -> fromAppJson reqBody
        ((_, accept) : _)
          | accept == appJson -> fromAppJson reqBody
          | otherwise -> fromAppJson reqBody

class Body h h' | h -> h' where
  body :: (MonadError Error m) => h -> Request -> m h'

instance {-# OVERLAPPABLE #-} (FromRequestBody a) => Body (a -> h') h' where
  body = mkExtractor (liftEither . left toErr . extractBody)
    where
      toErr err = Error InvalidArgument $ "Invalid request body: " ++ err

instance {-# OVERLAPPING #-} (FromRequestBody a) => Body (Either String a -> h') h' where
  body = mkExtractor (return . extractBody)
