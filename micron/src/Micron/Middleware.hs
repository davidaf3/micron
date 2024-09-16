module Micron.Middleware
  ( Middleware,
    MRoute,
    MPathPart (..),
    addMiddlewares,
    logReq,
    ($-/),
    (-/),
  )
where

import Data.ByteString.Char8 as BC (length, replicate, unpack)
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Micron.Request qualified as Request
import Micron.Routing
  ( Handler,
    Paths (..),
    SpecialPathKind (..),
    SpecialPaths (..),
  )
import Network.HTTP.Types as HTTP (Method, Status (statusCode))
import Network.Wai qualified as Wai
import Text.Printf (printf)

type Middleware a = Handler a -> Handler a

data MPath = MSpecialPath SpecialPathKind | MPathParts [MPathPart]

data MRoute a = MRoute Method MPath (Middleware a)

data MPathPart = MExact T.Text | Any | AnyAny

class ToMPath a where
  toMPath :: a -> MPath

instance ToMPath String where
  toMPath = MPathParts . toMPathParts

instance ToMPath MPathPart where
  toMPath = MPathParts . toMPathParts

instance ToMPath [MPathPart] where
  toMPath = MPathParts

instance ToMPath SpecialPathKind where
  toMPath = MSpecialPath

class ToMPathParts a where
  toMPathParts :: a -> [MPathPart]

instance ToMPathParts String where
  toMPathParts x = [MExact $ T.pack x]

instance ToMPathParts MPathPart where
  toMPathParts x = [x]

instance ToMPathParts [MPathPart] where
  toMPathParts = id

infixr 9 -/

(-/) :: (ToMPathParts a, ToMPathParts b) => a -> b -> [MPathPart]
x -/ ys = toMPathParts x ++ toMPathParts ys

infixl 1 $-/

($-/) :: (ToMPath a) => [Method] -> a -> Middleware b -> [MRoute b]
($-/) methods path m = map (\method -> MRoute method (toMPath path) m) methods

logReq :: Middleware a
logReq h req = do
  res <- h req
  time <- getCurrentTime
  let formattedTime = take 22 (iso8601Show time) ++ "Z"
      status = HTTP.statusCode $ Wai.responseStatus res
      method = Request.method req
      paddedMethod = BC.unpack $ method <> BC.replicate (7 - BC.length method) ' '
      path = BC.unpack $ Request.path req
  printf "%s\t%d\t%s\t%s\n" formattedTime status paddedMethod path
  return res

addMiddlewares ::
  [MRoute a] ->
  (Map Method (Paths a), Map Method (SpecialPaths a)) ->
  (Map Method (Paths a), Map Method (SpecialPaths a))
addMiddlewares routes byMethod = foldl doAdd byMethod $ reverse routes
  where
    doAdd (ps, sps) (MRoute method (MSpecialPath kind) m) =
      (ps, Map.update (Just . addSpecialMiddleware kind m) method sps)
    doAdd (ps, sps) (MRoute method (MPathParts path) m) =
      (Map.update (Just . addMiddleware path m) method ps, sps)

addSpecialMiddleware :: SpecialPathKind -> Middleware a -> SpecialPaths a -> SpecialPaths a
addSpecialMiddleware NotFoundPath m sps =
  SpecialPaths {notFoundHandler = m $ notFoundHandler sps}

addMiddleware :: [MPathPart] -> Middleware a -> Paths a -> Paths a
addMiddleware path m = addMiddleware' path False
  where
    updateChildren ps matchAll cs k = Map.update (Just . addMiddleware' ps matchAll) k cs
    addMiddleware' ps True (PathParam n h c) = PathParam n (h <&> m) $ addMiddleware' ps True c
    addMiddleware' ps True (PathTree h cs) = PathTree (h <&> m) $ foldl (updateChildren ps True) cs $ Map.keys cs
    addMiddleware' [] _ (PathParam n h c) = PathParam n (h <&> m) c
    addMiddleware' [] _ (PathTree h cs) = PathTree (h <&> m) cs
    addMiddleware' (p : ps) _ (PathParam n h c) = case p of
      Any -> PathParam n h $ addMiddleware' ps False c
      AnyAny -> PathParam n (h <&> m) $ addMiddleware' [p] True c
      _ -> PathParam n h $ addMiddleware' ps False c
    addMiddleware' (p : ps) _ (PathTree h cs) = case p of
      Any -> PathTree h $ foldl (updateChildren ps False) cs $ Map.keys cs
      AnyAny -> PathTree (h <&> m) $ foldl (updateChildren [p] True) cs $ Map.keys cs
      MExact n -> PathTree h $ updateChildren ps False cs n
