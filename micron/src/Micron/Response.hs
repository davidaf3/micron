{-# LANGUAGE DefaultSignatures #-}

module Micron.Response
  ( ToResponseContent (..),
    ok,
    created,
    notFound,
    badRequest,
    unauthorized,
    forbidden,
    unprocessableEntity,
    internalServerError,
  )
where

import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe, isJust)
import Data.String (IsString (fromString))
import Micron.MIMEType (appJson, textHtml, textPlain)
import Micron.Request (Request (headers))
import Network.HTTP.Types
  ( Status,
    hAccept,
    hContentType,
    status200,
    status201,
    status400,
    status401,
    status403,
    status404,
    status406,
    status422,
    status500,
  )
import Network.Wai qualified as Wai

class ToResponseContent a where
  toAppJson :: a -> Maybe BL.ByteString
  toTextHtml :: a -> Maybe BL.ByteString
  toTextPlain :: a -> Maybe BL.ByteString
  default toAppJson :: a -> Maybe BL.ByteString
  toAppJson _ = Nothing
  default toTextHtml :: a -> Maybe BL.ByteString
  toTextHtml _ = Nothing
  default toTextPlain :: a -> Maybe BL.ByteString
  toTextPlain _ = Nothing

instance ToResponseContent () where
  toAppJson = toTextPlain
  toTextHtml = toTextPlain
  toTextPlain _ = Just $ fromString ""

instance ToResponseContent String where
  toAppJson = toTextPlain
  toTextHtml = toTextPlain
  toTextPlain = Just . fromString

defaultAppJson :: BL.ByteString
defaultAppJson = fromString "{\"error\":\"Not Acceptable\"}"

defaultTextHtml :: BL.ByteString
defaultTextHtml =
  fromString
    "<!DOCTYPE html><html lang=\"en\">\
    \<head>\
    \<meta charset=\"UTF-8\">\
    \<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\
    \<title>Not Acceptable</title>\
    \</head>\
    \<body><h1>Not Acceptable</h1></body>\
    \</html>"

defaultTextPlain :: BL.ByteString
defaultTextPlain = fromString "Not Acceptable"

mkResponse :: (ToResponseContent a) => Status -> a -> Request p -> Wai.Response
mkResponse s x req =
  let (ct, def, content) = case filter ((== hAccept) . fst) $ headers req of
        [] -> (appJson, defaultAppJson, toAppJson x)
        ((_, accept) : _)
          | accept == appJson -> (appJson, defaultAppJson, toAppJson x)
          | accept == textHtml -> (textHtml, defaultTextHtml, toTextHtml x)
          | accept == textPlain -> (textPlain, defaultTextPlain, toTextPlain x)
          | otherwise -> (appJson, defaultAppJson, toAppJson x)
      status = if isJust content then s else status406
      ctHeader = (hContentType, ct)
   in Wai.responseLBS status [ctHeader] $ fromMaybe def content

ok :: (ToResponseContent a) => a -> Request p -> Wai.Response
ok = mkResponse status200

created :: (ToResponseContent a) => a -> Request p -> Wai.Response
created = mkResponse status201

badRequest :: (ToResponseContent a) => a -> Request p -> Wai.Response
badRequest = mkResponse status400

notFound :: (ToResponseContent a) => a -> Request p -> Wai.Response
notFound = mkResponse status404

unauthorized :: (ToResponseContent a) => a -> Request p -> Wai.Response
unauthorized = mkResponse status401

forbidden :: (ToResponseContent a) => a -> Request p -> Wai.Response
forbidden = mkResponse status403

unprocessableEntity :: (ToResponseContent a) => a -> Request p -> Wai.Response
unprocessableEntity = mkResponse status422

internalServerError :: (ToResponseContent a) => a -> Request p -> Wai.Response
internalServerError = mkResponse status500
