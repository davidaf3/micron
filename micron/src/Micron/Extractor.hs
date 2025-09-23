module Micron.Extractor ((~>), (~.), mkExtractor) where

import Control.Monad (join)

infixl 2 ~>

(~>) :: (Monad m) => (h -> r -> m (m o)) -> h -> r -> m o
(~>) exs h req = join (exs h req)

infixl 9 ~.

(~.) :: (Monad m) => (h -> r -> m h') -> (h' -> r -> m h'') -> h -> r -> m h''
(~.) exA exB h req = exA h req >>= flip exB req

mkExtractor :: (Monad m) => (r -> m a) -> (a -> h') -> r -> m h'
mkExtractor f h req = h <$> f req
