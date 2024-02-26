module Main (main) where

import Micron
  ( MPathPart (Any, AnyAny),
    SpecialPathKind (..),
    app,
    body,
    created,
    delete,
    errorRes,
    get,
    logReq,
    ok,
    param,
    post,
    put,
    query,
    ($-/),
    (-/),
    ($./),
    (./:),
    (~>),
    (~.),
    (|>),
  )
import Micron.Example.Auth (authenticated)
import Micron.Example.Resource.User.Service
  ( addUser,
    deleteUser,
    getUser,
    getUsers,
    updateUser,
  )
import Network.Wai.Handler.Warp qualified as Warp

main :: IO ()
main = do
  Warp.run 3000 $
    app
      [ get     $./ ""              $ const (return "Hello, World!") |> ok,
        get     $./ "user"          $ query ~> getUsers |> either errorRes ok,
        post    $./ "user"          $ body ~> addUser |> either errorRes created,
        get     $./ "user" ./: "id" $ param "id" ~> getUser |> either errorRes ok,
        put     $./ "user" ./: "id" $ param "id" ~. body ~> updateUser |> either errorRes ok,
        delete  $./ "user" ./: "id" $ param "id" ~> deleteUser |> maybe (ok "") errorRes
      ]
      [ [get, post, put, delete]  $-/ AnyAny        $ logReq,
        [get, post, put, delete]  $-/ NotFoundPath  $ logReq,
        [post, put, delete]       $-/ "user" -/ Any $ authenticated
      ]
