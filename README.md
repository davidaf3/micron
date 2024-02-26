# micron
micron is a micro web framework for Haskell.

# How to use
The following code shows how you can define a simple REST API using micron:

```haskell
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
        [post]                    $-/ "user" -/ Any $ authenticated,
        [put, delete]             $-/ "user" -/ Any $ authenticated
      ]
```

A full working example is available in the micron-example package.
