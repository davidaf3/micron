module Main (main) where

import Micron
  ( app,
    body,
    created,
    delete,
    get,
    logReq,
    ok,
    param,
    post,
    put,
    query,
    withMiddleware,
    defaultRoutes,
    ($./),
    (./),
    (./:),
    (|>),
    (~.),
    (~>),
  )
import Micron.Example.Auth (user, authenticated)
import Micron.Example.Resource.Post.Service
  ( addPost,
    deletePost,
    getPost,
    getPostsByUser,
    updatePost,
  )
import Micron.Example.Resource.User.Service
  ( getUsers,
    login,
    signUp,
  )
import Network.Wai.Handler.Warp qualified as Warp

main :: IO ()
main = do
  Warp.run 3000 $
    app $
      withMiddleware logReq $
        [ get   $./ "" $ const (return "Hello, World!") |> ok,
          post  $./ "sign-up" $ body ~> signUp |> created,
          post  $./ "login"   $ body ~> login |> ok,
          get   $./ "user"    $ query ~> getUsers |> ok,
          get   $./ "user" ./: "id" ./ "post" $ param "id" ~> getPostsByUser |> ok,
          get   $./ "post" ./: "id" $ param "id" ~> getPost |> ok
        ]
          ++ withMiddleware authenticated
            [ post    $./ "post"  $ body ~. user ~> addPost |> created,
              put     $./ "post" ./: "id" $ param "id" ~. body ~. user ~> updatePost |> ok,
              delete  $./ "post" ./: "id" $ param "id" ~. user ~> deletePost |> ok
            ]
          ++ defaultRoutes
