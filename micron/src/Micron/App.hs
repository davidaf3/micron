module Micron.App ((|>), app, defaultRoutes) where

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
    Route,
    SpecialPath (NotFoundPath),
    matchPath,
    matchSpecialPath,
    mkPaths,
    ($./),
  )
import Network.HTTP.Types.Method
  ( methodConnect,
    methodDelete,
    methodGet,
    methodHead,
    methodOptions,
    methodPatch,
    methodPost,
    methodPut,
    methodTrace,
  )
import Network.Wai qualified as Wai

infixr 1 |>

(|>) :: (Monad m) => (r -> m o) -> (o -> r -> Wai.Response) -> r -> m Wai.Response
(|>) h o req = h req <&> flip o req

app :: [Route IO] -> Wai.Request -> (Wai.Response -> IO b) -> IO b
app routes = app' $! mkPaths routes
  where
    app' (ps, sps) waiReq respond =
      let method = Wai.requestMethod waiReq
          path = case Wai.pathInfo waiReq of
            [] -> [T.empty]
            notEmpty -> notEmpty
          (h, args) = case matchPath method path ps of
            Just found -> found
            Nothing -> matchSpecialPath method NotFoundPath sps defaultNotFoundHandler
       in do
            req <- mkReq waiReq args
            res <- h req
            respond res

mkReq :: Wai.Request -> Map T.Text T.Text -> IO Request
mkReq waiReq args =
  let path = Wai.rawPathInfo waiReq
      method = Wai.requestMethod waiReq
      headers = Wai.requestHeaders waiReq
      cookies = getCookies headers
      queryString = Map.fromList $ mapMaybe mapQueryItem (Wai.queryString waiReq)
   in Wai.strictRequestBody waiReq <&> Request path method headers cookies args queryString
  where
    mapQueryItem (x, Just y) = Just (x, decodeUtf8 y)
    mapQueryItem (_, Nothing) = Nothing

defaultNotFoundHandler :: (Monad m) => Handler m
defaultNotFoundHandler = return . errorRes (Error NotFound "Not found")

defaultRoutes :: (Monad m) => [Route m]
defaultRoutes =
  let allMethods =
        [ methodConnect,
          methodDelete,
          methodGet,
          methodHead,
          methodOptions,
          methodPatch,
          methodPost,
          methodPut,
          methodTrace
        ]
   in concatMap (\method -> [method $./ NotFoundPath $ defaultNotFoundHandler]) allMethods
