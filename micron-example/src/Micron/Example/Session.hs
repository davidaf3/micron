{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Micron.Example.Session (session, emptySession) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', readTVar)
import Data.ByteString qualified as B (ByteString, concat)
import Data.Map (Map)
import Data.Map qualified as Map (empty, insert, lookup)
import Data.Maybe (fromMaybe)
import Data.Text.Encoding qualified as TE (encodeUtf8)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Micron (Middleware, Request (cookies))
import Micron.Polysemy.SafeIO (SafeIO, performSafeIO)
import Network.HTTP.Types.Header (hSetCookie)
import Network.Wai (mapResponseHeaders)
import Polysemy (Member, Sem)
import Polysemy.State (State, runState)

sessionKey :: B.ByteString
sessionKey = "micron-example-session"

emptySession :: Map B.ByteString s
emptySession = Map.empty

session ::
  (Member (SafeIO) r) =>
  TVar (Map B.ByteString s) ->
  s ->
  Middleware (Sem (State s ': r)) (Sem r)
session store def h req = do
  sId <- case Map.lookup sessionKey $ cookies req of
    Just sessionId -> return sessionId
    Nothing -> performSafeIO $ TE.encodeUtf8 . toText <$> nextRandom
  s <- performSafeIO $ atomically $ Map.lookup sId <$> readTVar store
  (s', res) <- runState (fromMaybe def s) (h req)
  _ <- performSafeIO $ atomically $ modifyTVar' store (Map.insert sId s')
  return $ mapResponseHeaders ((hSetCookie, B.concat [sessionKey, "=", sId]) :) res
