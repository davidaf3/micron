module Main (main) where

import Micron
  ( MPathPart (AnyAny),
    SpecialPathKind (..),
    app,
    body,
    extra,
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
    (|>),
    (~.),
    (./),
  )
import Micron.Example.Auth (authenticated)
import Micron.Example.Config (defaultRequestExtra, RequestExtra (user))
import Micron.Example.Resource.User.Service
  ( signUp,
    login,
    getUsers,
  )
import Micron.Example.Resource.Post.Service
  ( getPost,
    addPost,
    getPostsByUser,
    updatePost,
    deletePost,
  )
import Network.Wai.Handler.Warp qualified as Warp

main :: IO ()
main = do
  Warp.run 3000 $
    app
      [ get     $./ ""        $ const (return "Hello, World!") |> ok,
        post    $./ "sign-up" $ body ~> signUp |> either errorRes created,
        post    $./ "login"   $ body ~> login |> either errorRes ok,
        get     $./ "user"          $ query ~> getUsers |> either errorRes ok,
        get     $./ "user" ./: "id" ./ "post" $ param "id" ~> getPostsByUser |> either errorRes ok,
        post    $./ "post"          $ body ~. extra user ~> addPost |> either errorRes ok,
        get     $./ "post" ./: "id" $ param "id" ~> getPost |> either errorRes ok,
        put     $./ "post" ./: "id" $ param "id" ~. body ~. extra user ~> updatePost |> either errorRes ok,
        delete  $./ "post" ./: "id" $ param "id" ~. extra user ~> deletePost |> maybe (ok "") errorRes
      ]
      [ [get, post, put, delete]  $-/ AnyAny        $ logReq,
        [get, post, put, delete]  $-/ NotFoundPath  $ logReq,
        [post, put, delete]       $-/ "user" -/ AnyAny  $ authenticated,
        [post, put, delete]       $-/ "post" -/ AnyAny  $ authenticated
      ]
      defaultRequestExtra
