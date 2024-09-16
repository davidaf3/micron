# micron
micron is a micro web framework for Haskell.

# How to use
The following code shows how you can define a simple REST API using micron:

```haskell
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
```

A full working example is available in the micron-example package.
