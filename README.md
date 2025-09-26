# micron
micron is a micro web framework for Haskell.

# How to use
The following code shows how you can define a simple REST API using micron:

```haskell
main :: IO ()
main = do
  hits <- newTVarIO (emptySession @Int)
  Warp.run 3000 $
    app $
      middleware globalMiddleware $ do
        middleware (useSqlite "data/database.db") $ do
          get   $/ "" $ const (return "Hello, World!") |> ok
          post  $/ "sign-up"  $ body ~> signUp |> created
          post  $/ "login"    $ body ~> login |> ok
          get   $/ "user"     $ query ~> getUsers |> ok
          get   $/ "user" /: "id" / "post"  $ param "id" ~> getPostsByUser |> ok
          get   $/ "post" /: "id"           $ param "id" ~> getPost |> ok
          middleware authenticated $ do
            post    $/ "post"         $ body ~. user ~> addPost |> created
            put     $/ "post" /: "id" $ param "id" ~. body ~. user ~> updatePost |> ok
            delete  $/ "post" /: "id" $ param "id" ~. user ~> deletePost |> ok
        middleware (session hits 0) $ do
          get $/ "hit" $ const (modify @Int (+ 1) >> gets @Int show) |> ok
        defaultRoutes
```

A full working example is available in the micron-example package.
