module Micron.Util (mapRight) where

mapRight :: (a -> b) -> Either c a -> Either c b
mapRight f (Right x) = Right $ f x
mapRight _ (Left x) = Left x