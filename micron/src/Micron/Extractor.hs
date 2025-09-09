module Micron.Extractor (param, extra, query, body) where

import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Micron.Error (BaseErrorType (InvalidArgument), Error (Error))
import Micron.MIMEType (appJson)
import Micron.Request (FromQueryString (..), FromRequestBody (..), Parseable (..), Request (..))
import Network.HTTP.Types (hContentType)

param :: (Parseable a) => String -> (a -> b) -> Request c -> Either Error b
param name f req =
  let arg = Map.lookup (T.pack name) $ params req
   in case parseText $ fromMaybe T.empty arg of
        Right x -> Right $ f x
        Left err -> Left $ Error InvalidArgument $ "Invalid " ++ name ++ ": " ++ err

extra :: (c -> a) -> (a -> b) -> Request c -> Either Error b
extra accessor f req = Right $ f $ accessor $ extraData req

query :: (FromQueryString a) => (a -> b) -> Request c -> Either Error b
query f req = case fromQueryString $ queryString req of
  Right x -> Right $ f x
  Left _ -> Left $ Error InvalidArgument "Invalid query string"

body :: (FromRequestBody a) => (a -> b) -> Request c -> Either Error b
body f req =
  let mx = case filter ((== hContentType) . fst) $ headers req of
        [] -> fromAppJson $ requestBody req
        ((_, accept) : _)
          | accept == appJson -> fromAppJson $ requestBody req
          | otherwise -> fromAppJson $ requestBody req
   in case mx of
        Just x -> Right $ f x
        Nothing -> Left $ Error InvalidArgument "Invalid request body"
