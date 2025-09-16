module Micron.App ((|>), app, defaultRoutes) where

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Micron.Error (BaseErrorType (NotFound), Error (Error), ErrorType (responseMaker), errorRes)
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

app :: [Route (ExceptT Error IO)] -> Wai.Application
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
            eitherRes <- runExceptT (h req)
            respond $ case eitherRes of
              Right res -> res
              Left e@(Error t _) -> responseMaker t e req

mkReq :: Wai.Request -> Map T.Text T.Text -> IO Request
mkReq waiReq args =
  let path = Wai.rawPathInfo waiReq
      method = Wai.requestMethod waiReq
      headers = Wai.requestHeaders waiReq
      queryString = Map.fromList $ mapMaybe mapQueryItem (Wai.queryString waiReq)
   in Wai.strictRequestBody waiReq <&> Request path method headers args queryString
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
