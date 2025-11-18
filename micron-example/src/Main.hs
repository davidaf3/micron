{-# LANGUAGE TypeOperators #-}

import Control.Concurrent.STM (newTVarIO)
import Micron
  ( Error,
    Middleware,
    R,
    app,
    created,
    defaultRoutes,
    delete,
    get,
    middleware,
    ok,
    post,
    put,
    type (/),
    type (/:),
    (|>),
    (~.),
    (~>),
  )
import Micron.Example.Auth (authenticated, user)
import Micron.Example.Db (useSqlite)
import Micron.Example.Resource.Post.Service (addPost, deletePost, getPost, getPostsByUser, updatePost)
import Micron.Example.Resource.User.Service (getUsers, login, signUp)
import Micron.Example.Session (emptySession, session)
import Micron.Polysemy.Extractor (body, param, query)
import Micron.Polysemy.Middleware (handleAppErrors, logRequests, useLogging, usePolysemy, useSafeIO)
import Micron.Polysemy.SafeIO (SafeIO)
import Network.Wai.Handler.Warp qualified as Warp
import Polysemy (Sem)
import Polysemy.Error qualified as PE (Error)
import Polysemy.State (gets, modify)
import Polysemy.Trace (Trace, traceToStdout)
import Prelude hiding ((/))

globalMiddleware :: Middleware (Sem '[PE.Error Error, SafeIO, Trace]) IO
globalMiddleware = usePolysemy . useLogging traceToStdout . useSafeIO . logRequests . handleAppErrors

main :: IO ()
main = do
  hits <- newTVarIO (emptySession @Int)
  Warp.run 3000 $
    app $
      middleware globalMiddleware $ do
        middleware (useSqlite "data/database.db") $ do
          get   @(R / "") $ const (return "Hello, World!") |> ok
          post  @(R / "sign-up")  $ body ~> signUp |> created
          post  @(R / "login")    $ body ~> login |> ok
          get   @(R / "user")     $ query ~> getUsers |> ok
          get   @(R / "user" /: "id" / "post")  $ param @"id" ~> getPostsByUser |> ok
          get   @(R / "post" /: "id")           $ param @"id" ~> getPost |> ok
          middleware authenticated $ do
            post    @(R / "post") $ body ~. user ~> addPost |> created
            put     @(R / "post" /: "id") $ param @"id" ~. body ~. user ~> updatePost |> ok
            delete  @(R / "post" /: "id") $ param @"id" ~. user ~> deletePost |> ok
        middleware (session hits 0) $ do
          get @(R / "hit") $ const (modify @Int (+ 1) >> gets @Int show) |> ok
        defaultRoutes
