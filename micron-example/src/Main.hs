import Control.Concurrent.STM (newTVarIO)
import Micron
  ( Error,
    Middleware,
    app,
    created,
    defaultRoutes,
    delete,
    get,
    ok,
    post,
    put,
    withMiddleware,
    ($./),
    (./),
    (./:),
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

globalMiddleware :: Middleware (Sem '[PE.Error Error, SafeIO, Trace]) IO
globalMiddleware = usePolysemy . useLogging traceToStdout . useSafeIO . logRequests . handleAppErrors

main :: IO ()
main = do
  hits <- newTVarIO (emptySession @Int)
  Warp.run 3000 $
    app $
      withMiddleware globalMiddleware $
        withMiddleware (useSqlite "data/database.db") (
          [ get   $./ ""                        $ const (return "Hello, World!") |> ok,
            post  $./ "sign-up"                 $ body ~> signUp |> created,
            post  $./ "login"                   $ body ~> login |> ok,
            get   $./ "user"                    $ query ~> getUsers |> ok,
            get   $./ "user" ./: "id" ./ "post" $ param "id" ~> getPostsByUser |> ok,
            get   $./ "post" ./: "id"           $ param "id" ~> getPost |> ok
          ]
          ++ withMiddleware authenticated [
            post    $./ "post"          $ body ~. user ~> addPost |> created,
            put     $./ "post" ./: "id" $ param "id" ~. body ~. user ~> updatePost |> ok,
            delete  $./ "post" ./: "id" $ param "id" ~. user ~> deletePost |> ok
          ]
        )
        ++ withMiddleware (session hits 0) [
          get $./ "hit" $ const (modify @Int (+ 1) >> gets @Int show) |> ok
        ]
        ++ defaultRoutes
