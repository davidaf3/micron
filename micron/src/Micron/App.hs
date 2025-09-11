module Micron.App ((|>), (!|>), (~>), (!~>), (~.), app, defaultRoutes) where

import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Micron.Error (BaseErrorType (NotFound), Error (Error), ErrorType (responseMaker), errorRes)
import Micron.Request (BaseRequest (BaseRequest), Request)
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

infixl 2 ~>

(~>) :: (a -> r -> Either Error (IO b)) -> a -> r -> IO (Either Error b)
(~>) i h req = sequence $ i h req

infixl 2 !~>

(!~>) :: (a -> r -> Either Error (IO (Either Error b))) -> a -> r -> IO (Either Error b)
(!~>) i h req = either (return . Left) id $ i h req

infixr 1 |>

(|>) :: (r -> IO b) -> (b -> r -> Wai.Response) -> r -> IO Wai.Response
(|>) h o req = h req <&> flip o req

infixr 1 !|>

(!|>) :: (Request r) => (r -> IO (Either Error b)) -> (b -> r -> Wai.Response) -> r -> IO Wai.Response
(!|>) h o req = do
  ex <- h req
  return $ case ex of
    Right x -> o x req
    Left e@(Error t _) -> responseMaker t e req

infixl 9 ~.

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
          (h, args) = case matchPath method path ps of
            Just found -> found
            Nothing -> matchSpecialPath method NotFoundPath sps defaultNotFoundHandler
       in mkReq waiReq args >>= h >>= respond

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

defaultNotFoundHandler :: Handler BaseRequest
defaultNotFoundHandler = return . errorRes (Error NotFound "Not found")

defaultRoutes :: [Route BaseRequest]
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
