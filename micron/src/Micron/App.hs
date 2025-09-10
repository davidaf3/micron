module Micron.App ((|>), (!|>), (~>), (!~>), (~.), app) where

import Data.Bifunctor (Bifunctor (second))
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Micron.Error (Error (Error), ErrorType (responseMaker))
import Micron.Request (BaseRequest (BaseRequest), Request)
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

(~>) :: (a -> r -> Either Error (IO b)) -> a -> r -> IO (Either Error b)
(~>) i h req = sequence $ i h req

(!~>) :: (a -> r -> Either Error (IO (Either Error b))) -> a -> r -> IO (Either Error b)
(!~>) i h req = either (return . Left) id $ i h req

(|>) :: (r -> IO b) -> (b -> r -> Wai.Response) -> r -> IO Wai.Response
(|>) h o req = h req <&> flip o req

(!|>) :: (Request r) => (r -> IO (Either Error b)) -> (b -> r -> Wai.Response) -> r -> IO Wai.Response
(!|>) h o req = do
  ex <- h req
  return $ case ex of
    Right x -> o x req
    Left e@(Error t _) -> responseMaker t e req

(~.) :: (Monad m) => (a -> b -> m c) -> (c -> b -> m d) -> a -> b -> m d
(~.) f g h req = f h req >>= flip g req

app :: [Route BaseRequest] -> Wai.Application
app routes = app' $! mkPaths routes
  where
    app' (ps, sps) waiReq respond =
      let method = Wai.requestMethod waiReq
          path = case Wai.pathInfo waiReq of
            [] -> [T.empty]
            notEmpty -> notEmpty
          (h, args) = case matchRoute method path ps of
            Nothing -> (handleNotFound method sps, Map.empty)
            Just found -> second Map.fromList found
       in mkReq waiReq args >>= h >>= respond

handleNotFound :: Method -> Map Method SpecialPaths -> Handler BaseRequest
handleNotFound method sps =
  notFoundHandler
    (fromMaybe defaultSpecialPaths $ Map.lookup method sps)

mkReq :: Wai.Request -> Map T.Text T.Text -> IO BaseRequest
mkReq waiReq args =
  let path = Wai.rawPathInfo waiReq
      method = Wai.requestMethod waiReq
      headers = Wai.requestHeaders waiReq
      queryString = Map.fromList $ mapMaybe mapQueryItem (Wai.queryString waiReq)
   in Wai.strictRequestBody waiReq <&> BaseRequest path method headers args queryString
  where
    mapQueryItem (x, Just y) = Just (x, decodeUtf8 y)
    mapQueryItem (_, Nothing) = Nothing
