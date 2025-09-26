module Micron.Middleware (Middleware, middleware) where

import Control.Monad.State (modify, execState)
import Micron.Routing (Handler, Route (Route), Routes)

type Middleware m m' = Handler m -> Handler m'

middleware :: Middleware m m' -> Routes m -> Routes m'
middleware mi rs =
  let rs' = map applyMiddleware $ execState rs []
   in modify (rs' ++)
  where
    applyMiddleware (Route me p h) = Route me p (mi h)
