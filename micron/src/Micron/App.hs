module Micron.App ((|>), (!|>), (~>), (!~>), (~.), app) where

import Data.Bifunctor (Bifunctor (second))
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Micron.Error (Error (Error), ErrorType (responseMaker))
import Micron.Middleware (MRoute, addMiddlewares)
import Micron.Request (Request (Request))
import Micron.Routing
  ( Handler,
    Route,
    SpecialPaths (notFoundHandler),
    defaultSpecialPaths,
    matchRoute,
    mkPaths,
  )
import Network.HTTP.Types (Method)
import Network.Wai qualified as Wai

infixl 2 ~>

infixr 1 |>

infixr 1 !|>

infixl 9 ~.

(~>) :: (a -> Request c -> Either Error (IO b)) -> a -> Request c -> IO (Either Error b)
(~>) i h req = sequence $ i h req

(!~>) :: (a -> Request c -> Either Error (IO (Either Error b))) -> a -> Request c -> IO (Either Error b)
(!~>) i h req = either (return . Left) id $ i h req

(|>) :: (Request c -> IO b) -> (b -> Request c -> Wai.Response) -> Request c -> IO Wai.Response
(|>) h o req = h req <&> flip o req

(!|>) :: (Request c -> IO (Either Error b)) -> (b -> Request c -> Wai.Response) -> Request c -> IO Wai.Response
(!|>) h o req = do
  ex <- h req
  return $ case ex of
    Right x -> o x req
    Left e@(Error t _) -> responseMaker t e req

(~.) :: (Monad m) => (a -> b -> m c) -> (c -> b -> m d) -> a -> b -> m d
(~.) f g h req = f h req >>= flip g req

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
