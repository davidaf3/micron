module Micron.Routing
  ( get,
    post,
    put,
    delete,
    mkPaths,
    matchRoute,
    defaultSpecialPaths,
    (./),
    (./:),
    ($./),
    ($./:),
    Paths (..),
    SpecialPaths (..),
    SpecialPathKind (..),
    Path (..),
    Handler,
    Route (..),
  )
where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Micron.Error (BaseErrorType (..), Error (..), errorRes)
import Micron.Request (Request)
import Network.HTTP.Types.Method
  ( Method,
    methodConnect,
    methodDelete,
    methodGet,
    methodHead,
    methodOptions,
    methodPatch,
    methodPost,
    methodPut,
    methodTrace,
  )
import Network.Wai qualified as Wai

type Handler a = Request a -> IO Wai.Response

data SpecialPathKind = NotFoundPath

data Path = SpecialPath SpecialPathKind | PathParts [PathPart]

data Route a = Route Method Path (Handler a)

class ToPath a where
  toPath :: a -> Path

instance ToPath String where
  toPath = PathParts . fromExact

instance ToPath SpecialPathKind where
  toPath = SpecialPath

instance ToPath [PathPart] where
  toPath = PathParts

instance ToPath (String, [PathPart]) where
  toPath = PathParts . fromExact

data Paths a
  = PathTree (Maybe (Handler a)) (Map T.Text (Paths a))
  | PathParam (Set T.Text) (Maybe (Handler a)) (Paths a)

newtype SpecialPaths a = SpecialPaths {notFoundHandler :: Handler a}

data PathPart = Exact T.Text | Param T.Text

class ToPathParts a where
  fromExact :: a -> [PathPart]
  fromParam :: a -> [PathPart]

instance ToPathParts String where
  fromExact x = [Exact $ T.pack x]
  fromParam x = [Param $ T.pack x]

instance ToPathParts (String, [PathPart]) where
  fromExact (x, pps) = Exact (T.pack x) : pps
  fromParam (x, pps) = Param (T.pack x) : pps

infixr 9 ./

(./) :: (ToPathParts a) => String -> a -> (String, [PathPart])
x ./ ys = (x, fromExact ys)

infixr 9 ./:

(./:) :: (ToPathParts a) => String -> a -> (String, [PathPart])
x ./: ys = (x, fromParam ys)

infixl 8 $./

($./) :: (ToPath a) => Method -> a -> (Handler b -> Route b)
method $./ path = Route method $ toPath path

infixl 8 $./:

($./:) :: (ToPathParts a) => Method -> a -> (Handler b -> Route b)
method $./: path = Route method $ toPath $ fromParam path

defaultSpecialPaths :: SpecialPaths a
defaultSpecialPaths =
  SpecialPaths
    { notFoundHandler = return . errorRes (Error NotFound "Not found")
    }

defaultSpecialPathsByMethod :: Map Method (SpecialPaths a)
defaultSpecialPathsByMethod =
  let methods =
        [ methodConnect,
          methodDelete,
          methodGet,
          methodHead,
          methodOptions,
          methodPatch,
          methodPost,
          methodPut,
          methodTrace
        ]
   in Map.fromList $ map (,defaultSpecialPaths) methods

get :: Method
get = methodGet

post :: Method
post = methodPost

put :: Method
put = methodPut

delete :: Method
delete = methodDelete

mkPaths :: [Route a] -> (Map Method (Paths a), Map Method (SpecialPaths a))
mkPaths = foldl insertPath (Map.empty, defaultSpecialPathsByMethod)
  where
    insertPath (ps, sps) (Route method (SpecialPath kind) h) =
      (ps, Map.alter (Just . addSpecialPath kind h) method sps)
    insertPath (ps, sps) (Route method (PathParts path) h) =
      (Map.alter (Just . addPath path h) method ps, sps)

addSpecialPath :: SpecialPathKind -> Handler a -> Maybe (SpecialPaths a) -> SpecialPaths a
addSpecialPath kind h Nothing = addSpecialPath kind h $ Just defaultSpecialPaths
addSpecialPath NotFoundPath h (Just sps) = sps {notFoundHandler = h}

addPath :: [PathPart] -> Handler a -> Maybe (Paths a) -> Paths a
addPath path h Nothing = addPath path h $ Just (PathTree Nothing Map.empty)
addPath path h (Just root) = addPath' path root
  where
    addEmpty ps = addPath' ps $ PathTree Nothing Map.empty
    addPath' [] (PathParam n _ c) = PathParam n (Just h) c
    addPath' [] (PathTree _ cs) = PathTree (Just h) cs
    addPath' (p : ps) (PathParam ns ph c) = case p of
      Param n -> PathParam (Set.insert n ns) ph $ addPath' ps c
      _ -> addEmpty ps
    addPath' (p : ps) (PathTree ph cs) = case p of
      Param n -> PathParam (Set.singleton n) ph $ addEmpty ps
      Exact n -> PathTree ph $ Map.insertWith (const $ addPath' ps) n (addEmpty ps) cs

matchRoute :: Method -> [T.Text] -> Map Method (Paths a) -> Maybe (Handler a, [(T.Text, T.Text)])
matchRoute method path byMethod = Map.lookup method byMethod >>= matchRoute' [] path
  where
    matchRoute' _ [] (PathParam _ Nothing _) = Nothing
    matchRoute' _ [] (PathTree Nothing _) = Nothing
    matchRoute' args [] (PathParam _ (Just h) _) = Just (h, args)
    matchRoute' args [] (PathTree (Just h) _) = Just (h, args)
    matchRoute' args (p : ps) (PathParam ns _ c) = matchRoute' (args ++ map (,p) (Set.elems ns)) ps c
    matchRoute' args (p : ps) (PathTree _ cs) = Map.lookup p cs >>= matchRoute' args ps
