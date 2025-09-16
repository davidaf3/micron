module Micron.Middleware (Middleware, withMiddleware, logReq) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Char8 as BC (length, replicate, unpack)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Micron.Request qualified as Request
import Micron.Routing (Handler, Route (Route))
import Network.HTTP.Types as HTTP (Status (statusCode))
import Network.Wai qualified as Wai
import Text.Printf (printf)

type Middleware m m' = Handler m -> Handler m'

withMiddleware :: Middleware m m' -> [Route m] -> [Route m']
withMiddleware mi = map (\(Route me p h) -> Route me p (mi h))

logReq :: (MonadIO m) => Middleware m m
logReq h req = do
  res <- h req
  time <- liftIO getCurrentTime
  let formattedTime = take 22 (iso8601Show time) ++ "Z"
      status = HTTP.statusCode $ Wai.responseStatus res
      method = Request.method req
      paddedMethod = BC.unpack $ method <> BC.replicate (7 - BC.length method) ' '
      path = BC.unpack $ Request.path req
  liftIO $ printf "%s\t%d\t%s\t%s\n" formattedTime status paddedMethod path
  return res
