module Micron
  ( Request.Request (..),
    (./),
    (./:),
    ($./),
    ($./:),
    (-/),
    (|>),
    (~>),
    (~.),
    ($-/),
    FromRequestBody (..),
    FromQueryString (..),
    SpecialPathKind (..),
    body,
    param,
    extra,
    query,
    parseText,
    app,
    get,
    post,
    put,
    delete,
    mkPaths,
    MPathPart (Any, AnyAny),
    module Micron.Response,
    Middleware,
    logReq,
    module Micron.Error,
  )
where

import Data.Bifunctor (Bifunctor (second))
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Micron.Error (BaseError, BaseErrorType (..), Error (..), errorRes)
import Micron.Middleware
  ( MPathPart (..),
    MRoute,
    Middleware,
    addMiddlewares,
    logReq,
    ($-/),
    (-/),
  )
import Micron.Request
  ( FromQueryString (..),
    FromRequestBody (..),
    Request (Request),
    body,
    extra,
    param,
    parseText,
    query,
  )
import Micron.Request qualified as Request
import Micron.Response
import Micron.Routing
  ( Handler,
    Route,
    SpecialPathKind (..),
    SpecialPaths (notFoundHandler),
    defaultSpecialPaths,
    delete,
    get,
    matchRoute,
    mkPaths,
    post,
    put,
    ($./),
    ($./:),
    (./),
    (./:),
  )
import Network.HTTP.Types (Method)
import Network.Wai qualified as Wai

infixl 2 ~>

infixr 1 |>

infixl 9 ~.

(~>) :: ((Request c -> a) -> Request c -> b) -> a -> Request c -> b
(~>) i h = i (const h)

(|>) :: (Request c -> IO b) -> (b -> Request c -> Wai.Response) -> Request c -> IO Wai.Response
(|>) h o req = h req <&> flip o req

(~.) :: (a -> b) -> (b -> c) -> a -> c
f ~. g = g . f

app :: [Route a] -> [[MRoute a]] -> a -> Wai.Application
app hRoutes mRoutes defaultExtra = app' $! addMiddlewares (concat mRoutes) $! mkPaths hRoutes
  where
    app' (ps, sps) waiReq respond =
      let method = Wai.requestMethod waiReq
          path = case Wai.pathInfo waiReq of
            [] -> [T.empty]
            notEmpty -> notEmpty
          (h, args) = case matchRoute method path ps of
            Nothing -> (handleNotFound method sps, Map.empty)
            Just found -> second Map.fromList found
       in mkReq waiReq args defaultExtra >>= h >>= respond

handleNotFound :: Method -> Map Method (SpecialPaths a) -> Handler a
handleNotFound method sps =
  notFoundHandler
    (fromMaybe defaultSpecialPaths $ Map.lookup method sps)

mkReq :: Wai.Request -> Map T.Text T.Text -> a -> IO (Request a)
mkReq waiReq args extraData =
  let path = Wai.rawPathInfo waiReq
      method = Wai.requestMethod waiReq
      headers = Wai.requestHeaders waiReq
      queryString = Map.fromList $ mapMaybe mapQueryItem (Wai.queryString waiReq)
   in Wai.strictRequestBody waiReq <&> Request path method headers args queryString extraData
  where
    mapQueryItem (x, Just y) = Just (x, decodeUtf8 y)
    mapQueryItem (_, Nothing) = Nothing
