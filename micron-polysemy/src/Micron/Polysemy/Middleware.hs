{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Micron.Polysemy.Middleware
  ( usePolysemy,
    useLogging,
    logRequests,
    handleAppErrors,
    useSafeIO,
  )
where

import Data.ByteString.Char8 as BC (length, replicate, unpack)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Micron (BaseErrorType (Unexpected), Error (Error), Middleware, errorRes)
import Micron qualified as Request (Request (..))
import Micron.Polysemy.SafeIO (SafeIO, ioToSafeIO, performSafeIO)
import Network.HTTP.Types as HTTP (Status (statusCode))
import Network.Wai qualified as Wai
import Polysemy (Embed, InterpreterFor, Member, Members, Sem, runM, subsume_)
import Polysemy.Error (runError)
import Polysemy.Error qualified as PE (Error)
import Polysemy.Internal (Subsume)
import Polysemy.Trace (Trace, trace)
import Text.Printf (printf)

usePolysemy :: (Monad m) => Middleware (Sem '[Embed m]) m
usePolysemy h req = runM (h req)

useLogging :: InterpreterFor Trace r -> Middleware (Sem (Trace ': r)) (Sem r)
useLogging runTrace h req = runTrace (h req)

logRequests :: (Members '[Trace, SafeIO] r) => Middleware (Sem r) (Sem r)
logRequests h req = do
  res <- h req
  time <- performSafeIO getCurrentTime
  let formattedTime = take 22 (iso8601Show time) ++ "Z"
      status = HTTP.statusCode $ Wai.responseStatus res
      method = Request.method req
      paddedMethod = BC.unpack $ method <> BC.replicate (7 - BC.length method) ' '
      path = BC.unpack $ Request.rawPath req
  trace $ printf "%s\t%d\t%s\t%s" formattedTime status paddedMethod path
  return res

handleAppErrors :: Middleware (Sem (PE.Error Error ': r)) (Sem r)
handleAppErrors h req = do
  eitherRes <- runError (h req)
  return $ case eitherRes of
    Right res -> res
    Left err -> errorRes err req

useSafeIO :: (Member Trace r, Subsume (Embed IO ': r) r') => Middleware (Sem (SafeIO ': r)) (Sem r')
useSafeIO h req = subsume_ $ do
  eitherRes <- runError (ioToSafeIO (h req))
  case eitherRes of
    Right res -> return res
    Left e -> do
      trace $ show e
      return $ errorRes (Error Unexpected "Unexpected error") req
