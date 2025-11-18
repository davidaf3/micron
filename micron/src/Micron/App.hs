{-# LANGUAGE TypeApplications #-}

module Micron.App ((|>), app, defaultRoutes) where

import Control.Monad (forM_)
import Control.Monad.State (execState)
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Micron.Cookie (getCookies)
import Micron.Error (BaseErrorType (NotFound), Error (Error), errorRes)
import Micron.Request (Request (Request))
import Micron.Routing
  ( Handler,
    Path (Special),
    Route (Route),
    Routes,
    SpecialPath (NotFoundPath),
    allMethods,
    matchPath,
    matchSpecialPath,
    mkPaths,
    mkRoute,
  )
import Micron.Sing.Path (SDPath)
import Network.Wai qualified as Wai

infixr 1 |>

(|>) :: (Monad m) => (r -> m o) -> (o -> r -> Wai.Response) -> r -> m Wai.Response
(|>) h o req = h req <&> flip o req

app :: Routes IO -> Wai.Request -> (Wai.Response -> IO b) -> IO b
app routes waiReq respond = app' $! mkPaths $ execState routes []
  where
    app' (ps, sps) =
      let method = Wai.requestMethod waiReq
          rawPath = case Wai.pathInfo waiReq of
            [] -> [T.empty]
            notEmpty -> notEmpty
          (route, args) = case matchPath method rawPath ps of
            Just found -> found
            Nothing -> matchSpecialPath method sps defaultNotFoundHandler
       in handle route args
    handle (Route _ path handler) args = do
      req <- mkReq waiReq args path
      res <- handler req
      respond res

mkReq :: Wai.Request -> Map T.Text T.Text -> SDPath p -> IO (Request p)
mkReq waiReq args _ =
  let rawPath = Wai.rawPathInfo waiReq
      method = Wai.requestMethod waiReq
      headers = Wai.requestHeaders waiReq
      cookies = getCookies headers
      queryString = Map.fromList $ mapMaybe mapQueryItem (Wai.queryString waiReq)
   in Wai.strictRequestBody waiReq <&> Request rawPath method headers cookies args queryString
  where
    mapQueryItem (x, Just y) = Just (x, decodeUtf8 y)
    mapQueryItem (_, Nothing) = Nothing

defaultNotFoundHandler :: (Monad m) => Handler ('Special 'NotFoundPath) m
defaultNotFoundHandler = return . errorRes (Error NotFound "Not found")

defaultRoutes :: (Monad m) => Routes m
defaultRoutes = forM_ allMethods $ \method -> do
  mkRoute @NotFoundPath method defaultNotFoundHandler
