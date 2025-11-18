{-# LANGUAGE RankNTypes #-}

module Micron.Middleware (Middleware, middleware) where

import Control.Monad.State (execState, modify)
import Micron.Routing (Handler, Route (Route), Routes)

type Middleware m m' = forall p. Handler p m -> Handler p m'

middleware :: Middleware m m' -> Routes m -> Routes m'
middleware mi rs =
  let rs' = map applyMiddleware $ execState rs []
   in modify (rs' ++)
  where
    applyMiddleware (Route me p h) = Route me p (mi h)
