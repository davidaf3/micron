{-# LANGUAGE FlexibleContexts #-}

module Micron.Polysemy.Extractor (param, paramE, query, queryE, body, bodyE) where

import Control.Arrow (ArrowChoice (left))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Micron
  ( BaseErrorType (InvalidArgument),
    Error (Error),
    FromQueryString (..),
    FromRequestBody (..),
    Parseable (..),
    Request (..),
    appJson,
    mkExtractor,
  )
import Network.HTTP.Types (hContentType)
import Polysemy (Member, Sem)
import Polysemy.Error (fromEither)
import Polysemy.Error qualified as PE (Error)

extractParam :: (Parseable a) => String -> (Request -> Either String a)
extractParam name req =
  let arg = Map.lookup (T.pack name) $ params req
   in parseText $ fromMaybe T.empty arg

param :: (Parseable a, Member (PE.Error Error) r) => String -> (a -> h') -> Request -> Sem r h'
param name = mkExtractor (fromEither . left toErr . extractParam name)
  where
    toErr err = Error InvalidArgument $ "Invalid " ++ name ++ ": " ++ err

paramE :: (Parseable a) => String -> (Either String a -> h') -> Request -> Sem r h'
paramE name = mkExtractor (return . extractParam name)

extractQuery :: (FromQueryString a) => Request -> Either (Map String String) a
extractQuery = fromQueryString . queryString

query :: (FromQueryString a, Member (PE.Error Error) r) => (a -> h') -> Request -> Sem r h'
query = mkExtractor (fromEither . left toErr . extractQuery)
  where
    toErr err = Error InvalidArgument $ "Invalid query string: " ++ show err

queryE :: (FromQueryString a) => (Either (Map String String) a -> h') -> Request -> Sem r h'
queryE = mkExtractor (return . extractQuery)

extractBody :: (FromRequestBody a) => Request -> Either String a
extractBody req =
  let reqBody = requestBody req
   in case filter ((== hContentType) . fst) $ headers req of
        [] -> fromAppJson reqBody
        ((_, accept) : _)
          | accept == appJson -> fromAppJson reqBody
          | otherwise -> fromAppJson reqBody

body :: (FromRequestBody a, Member (PE.Error Error) r) => (a -> h') -> Request -> Sem r h'
body = mkExtractor (fromEither . left toErr . extractBody)
  where
    toErr err = Error InvalidArgument $ "Invalid request body: " ++ err

bodyE :: (FromRequestBody a) => (Either String a -> h') -> Request -> Sem r h'
bodyE = mkExtractor (return . extractBody)
