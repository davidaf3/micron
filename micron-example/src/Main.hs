{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main (main) where

import Micron
  ( app,
    body,
    created,
    delete,
    extra,
    get,
    logReq,
    ok,
    param,
    post,
    put,
    query,
    withMiddleware,
    (!|>),
    (!~>),
    ($./),
    (./),
    (./:),
    (|>),
    (~.),
    (~>),
  )
import Micron.Example.Auth (authenticated)
import Micron.Example.Config (RequestExtra (user), defaultRequestExtra)
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
    app
      ( withMiddleware logReq $
          [ get   $./ "" $ const (return "Hello, World!") |> ok,
            post  $./ "sign-up" $ body !~> signUp !|> created,
            post  $./ "login"   $ body !~> login !|> ok,
            get   $./ "user"    $ query ~> getUsers !|> ok,
            get   $./ "user" ./: "id" ./ "post" $ param "id" ~> getPostsByUser !|> ok,
            get   $./ "post" ./: "id" $ param "id" !~> getPost !|> ok
          ]
            ++ withMiddleware authenticated
              [ post    $./ "post"  $ body ~. extra user !~> addPost !|> created,
                put     $./ "post" ./: "id" $ param "id" ~. body ~. extra user !~> updatePost !|> ok,
                delete  $./ "post" ./: "id" $ param "id" ~. extra user !~> deletePost !|> ok
              ]
      )
      defaultRequestExtra
