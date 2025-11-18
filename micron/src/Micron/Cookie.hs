{-# LANGUAGE OverloadedStrings #-}

module Micron.Cookie (getCookies) where

import Data.Attoparsec.ByteString.Char8 (Parser, char, parseOnly, sepBy, skipSpace, takeTill)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC (strip)
import Data.Either (fromRight)
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as Map (empty, fromList)
import Network.HTTP.Types (Header)

cookie :: Parser (B.ByteString, B.ByteString)
cookie = do
  name <- takeTill (== '=')
  _ <- char '='
  value <- takeTill (\c -> c == ';' || c == ' ')
  return (BC.strip name, value)

cookies :: Parser (Map B.ByteString B.ByteString)
cookies = do
  pairs <- cookie `sepBy` (skipSpace *> char ';' *> skipSpace)
  return (Map.fromList pairs)

parseCookieHeader :: B.ByteString -> Map B.ByteString B.ByteString
parseCookieHeader = fromRight Map.empty . parseOnly cookies

getCookies :: [Header] -> Map B.ByteString B.ByteString
getCookies headers =
  let cookieHeader = find ((== "Cookie") . fst) headers
   in maybe Map.empty (parseCookieHeader . snd) cookieHeader
