{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Micron.Polysemy.Extractor (param, paramE, query, queryE, body, bodyE) where

import Control.Arrow (ArrowChoice (left))
import Data.Data (Proxy (Proxy))
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import GHC.TypeLits (KnownSymbol, symbolVal)
import Micron
  ( BaseErrorType (InvalidArgument),
    Error (Error),
    FromQueryString (..),
    FromRequestBody (..),
    HasParam,
    Parseable (..),
    Request (..),
    appJson,
    getParam,
    mkExtractor,
  )
import Network.HTTP.Types (hContentType)
import Polysemy (Member, Sem)
import Polysemy.Error (fromEither)
import Polysemy.Error qualified as PE (Error)

extractParam ::
  forall param path a.
  (KnownSymbol param, Parseable a, HasParam path param ~ 'True) =>
  (Request path -> Either String a)
extractParam req = parseText $ fromMaybe T.empty $ getParam @param req

param ::
  forall param path a r h'.
  (KnownSymbol param, Parseable a, Member (PE.Error Error) r, HasParam path param ~ 'True) =>
  (a -> h') ->
  Request path ->
  Sem r h'
param = mkExtractor (fromEither . left toErr . extractParam @param)
  where
    toErr err = Error InvalidArgument $ "Invalid " ++ symbolVal (Proxy @param) ++ ": " ++ err

paramE ::
  forall param path a r h'.
  (KnownSymbol param, Parseable a, HasParam path param ~ 'True) =>
  (Either String a -> h') ->
  Request path ->
  Sem r h'
paramE = mkExtractor (return . extractParam @param)

extractQuery :: (FromQueryString a) => Request p -> Either (Map String String) a
extractQuery = fromQueryString . queryString

query :: (FromQueryString a, Member (PE.Error Error) r) => (a -> h') -> Request p -> Sem r h'
query = mkExtractor (fromEither . left toErr . extractQuery)
  where
    toErr err = Error InvalidArgument $ "Invalid query string: " ++ show err

queryE :: (FromQueryString a) => (Either (Map String String) a -> h') -> Request p -> Sem r h'
queryE = mkExtractor (return . extractQuery)

extractBody :: (FromRequestBody a) => Request p -> Either String a
extractBody req =
  let reqBody = requestBody req
   in case filter ((== hContentType) . fst) $ headers req of
        [] -> fromAppJson reqBody
        ((_, accept) : _)
          | accept == appJson -> fromAppJson reqBody
          | otherwise -> fromAppJson reqBody

body :: (FromRequestBody a, Member (PE.Error Error) r) => (a -> h') -> Request p -> Sem r h'
body = mkExtractor (fromEither . left toErr . extractBody)
  where
    toErr err = Error InvalidArgument $ "Invalid request body: " ++ err

bodyE :: (FromRequestBody a) => (Either String a -> h') -> Request p -> Sem r h'
bodyE = mkExtractor (return . extractBody)
