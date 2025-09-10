module Micron.Extractor (param, query, body) where

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

param :: (Parseable a, Request r) => String -> (a -> b) -> r -> Either Error b
param name f req =
  let arg = Map.lookup (T.pack name) $ params $ getBaseRequest req
   in case parseText $ fromMaybe T.empty arg of
        Right x -> Right $ f x
        Left err -> Left $ Error InvalidArgument $ "Invalid " ++ name ++ ": " ++ err

query :: (FromQueryString a, Request r) => (a -> b) -> r -> Either Error b
query f req = case fromQueryString $ queryString $ getBaseRequest req of
  Right x -> Right $ f x
  Left _ -> Left $ Error InvalidArgument "Invalid query string"

body :: (FromRequestBody a, Request r) => (a -> b) -> r -> Either Error b
body f req =
  let reqBody = requestBody $ getBaseRequest req
      mx = case filter ((== hContentType) . fst) $ headers $ getBaseRequest req of
        [] -> fromAppJson reqBody
        ((_, accept) : _)
          | accept == appJson -> fromAppJson reqBody
          | otherwise -> fromAppJson reqBody
   in case mx of
        Just x -> Right $ f x
        Nothing -> Left $ Error InvalidArgument "Invalid request body"
