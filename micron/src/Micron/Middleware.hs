module Micron.Middleware (Middleware, withMiddleware) where

import Micron.Routing (Handler, Route (Route))

type Middleware m m' = Handler m -> Handler m'

withMiddleware :: Middleware r r' -> [Route r] -> [Route r']
withMiddleware mi = map (\(Route me p h) -> Route me p (mi h))
