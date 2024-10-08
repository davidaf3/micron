{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Micron.Request
  ( Request (..),
    FromRequestBody (..),
    FromQueryString (..),
    body,
    param,
    query,
    extra,
    parseText,
  )
where

import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Either (partitionEithers)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import Data.Text qualified as T
import Data.Text.Read (decimal)
import Data.Void (Void)
import GHC.Generics
  ( C,
    D,
    Generic (Rep, to),
    K1 (..),
    M1 (..),
    S,
    Selector (selName),
    (:*:) (..),
  )
import Micron.MIMEType (appJson)
import Micron.Util (mapRight)
import Network.HTTP.Types (Method, RequestHeaders, hContentType)

type QueryString = Map B.ByteString T.Text

data Request a = Request
  { path :: B.ByteString,
    method :: Method,
    headers :: RequestHeaders,
    params :: Map T.Text T.Text,
    queryString :: QueryString,
    extraData :: a,
    requestBody :: BL.ByteString
  }

class FromRequestBody a where
  fromAppJson :: BL.ByteString -> Maybe a
  default fromAppJson :: BL.ByteString -> Maybe a
  fromAppJson _ = Nothing

class Parseable a where
  parseText :: T.Text -> Either String a

instance Parseable T.Text where
  parseText text
    | text == T.empty = Left "Empty value"
    | otherwise = Right text

instance Parseable Int where
  parseText x = case decimal x of
    (Right (parsed, rest)) | rest == T.empty -> Right parsed
    _ -> Left "Invalid integer format"

instance Parseable Void where
  parseText _ = Left "Value should not be present"

instance (Parseable a) => Parseable [a] where
  parseText text
    | text == T.empty = Left "Empty value"
    | otherwise = case partitionEithers $ map parseText $ T.split (== ',') text of
        ([], rights) -> Right rights
        _ -> Left "Invalid list format"

instance (Parseable a) => Parseable (Maybe a) where
  parseText text
    | text == T.empty = Right Nothing
    | otherwise = mapRight Just $ parseText text

class FromQueryString a where
  fromQueryString :: QueryString -> Either (Map String String) a
  default fromQueryString :: (Generic a, GFromQueryString (Rep a)) => QueryString -> Either (Map String String) a
  fromQueryString qs = mapRight to $ gfromQueryString qs

class GFromQueryString f where
  gfromQueryString :: QueryString -> Either (Map String String) (f a)

instance (GFromQueryString a, GFromQueryString b) => GFromQueryString (a :*: b) where
  gfromQueryString qs = case (gfromQueryString qs, gfromQueryString qs) of
    (Right x, Right y) -> Right $ x :*: y
    (Left errs, Right _) -> Left errs
    (Right _, Left errs) -> Left errs
    (Left xErrs, Left yErrs) -> Left $ Map.union xErrs yErrs

instance (GFromQueryString a) => GFromQueryString (M1 D c a) where
  gfromQueryString qs = mapRight M1 $ gfromQueryString qs

instance (GFromQueryString a) => GFromQueryString (M1 C c a) where
  gfromQueryString qs = mapRight M1 $ gfromQueryString qs

instance (Parseable a, Selector c) => GFromQueryString (M1 S c (K1 i a)) where
  gfromQueryString qs =
    let sel = selName (undefined :: M1 S c (K1 i a) p)
        val = fromMaybe T.empty $ Map.lookup (fromString sel) qs
     in case parseText val of
          Right x -> Right $ M1 $ K1 x
          Left err -> Left $ Map.singleton sel err

param :: (Parseable a) => String -> (Request c -> Either String a -> b) -> Request c -> b
param name f req =
  let arg = Map.lookup (T.pack name) $ params req
   in f req $ parseText $ fromMaybe T.empty arg

query :: (FromQueryString a) => (Request c -> Either (Map String String) a -> b) -> Request c -> b
query f req = f req $ fromQueryString $ queryString req

body :: (FromRequestBody a) => (Request c -> Maybe a -> b) -> Request c -> b
body f req =
  let x = case filter ((== hContentType) . fst) $ headers req of
        [] -> fromAppJson $ requestBody req
        ((_, accept) : _)
          | accept == appJson -> fromAppJson $ requestBody req
          | otherwise -> fromAppJson $ requestBody req
   in f req x

extra :: (a -> c) -> (Request a -> c -> b) -> Request a -> b
extra accessor f req = f req $ accessor $ extraData req
