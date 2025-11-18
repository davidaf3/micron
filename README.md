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
```

A full working example is available in the micron-example package.
