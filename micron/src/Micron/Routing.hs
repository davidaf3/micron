module Micron.Routing
  ( get,
    post,
    put,
    delete,
    mkPaths,
    matchPath,
    matchSpecialPath,
    (./),
    (./:),
    ($./),
    ($./:),
    Paths (..),
    SpecialPath (..),
    Path (..),
    Handler,
    Route (..),
  )
where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Micron.Request (Request)
import Network.HTTP.Types.Method
  ( Method,
    methodDelete,
    methodGet,
    methodPost,
    methodPut,
  )
import Network.Wai qualified as Wai

type Handler m = Request -> m Wai.Response

data SpecialPath = NotFoundPath deriving (Eq, Ord)

data Path = SpecialPath SpecialPath | PathParts [PathPart]

data Route m = Route Method Path (Handler m)

class ToPath a where
  toPath :: a -> Path

instance ToPath String where
  toPath = PathParts . fromExact

instance ToPath SpecialPath where
  toPath = SpecialPath

instance ToPath [PathPart] where
  toPath = PathParts

instance ToPath (String, [PathPart]) where
  toPath = PathParts . fromExact

data Paths m
  = PathTree (Maybe (Handler m)) (Map T.Text (Paths m))
  | PathParam (Set T.Text) (Maybe (Handler m)) (Paths m)

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

($./) :: (ToPath a) => Method -> a -> (Handler m -> Route m)
method $./ path = Route method $ toPath path

infixl 8 $./:

($./:) :: (ToPathParts a) => Method -> a -> (Handler m -> Route m)
method $./: path = Route method $ toPath $ fromParam path

get :: Method
get = methodGet

post :: Method
post = methodPost

put :: Method
put = methodPut

delete :: Method
delete = methodDelete

type PathsMap m = Map Method (Paths m)

type SpecialPathsMap m = Map (Method, SpecialPath) (Handler m)

mkPaths :: [Route m] -> (PathsMap m, SpecialPathsMap m)
mkPaths = foldl insertPath (Map.empty, Map.empty)
  where
    insertPath (ps, sps) (Route method (SpecialPath sp) h) = (ps, Map.insert (method, sp) h sps)
    insertPath (ps, sps) (Route method (PathParts path) h) =
      (Map.alter (Just . addPath path h) method ps, sps)

addPath :: [PathPart] -> Handler m -> Maybe (Paths m) -> Paths m
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

matchPath :: Method -> [T.Text] -> PathsMap m -> Maybe (Handler m, Map T.Text T.Text)
matchPath method path pathsMap = do
  ps <- Map.lookup method pathsMap
  (h, args) <- matchPath' [] path ps
  return (h, Map.fromList args)
  where
    matchPath' _ [] (PathParam _ Nothing _) = Nothing
    matchPath' _ [] (PathTree Nothing _) = Nothing
    matchPath' args [] (PathParam _ (Just h) _) = Just (h, args)
    matchPath' args [] (PathTree (Just h) _) = Just (h, args)
    matchPath' args (p : ps) (PathParam ns _ c) = matchPath' (args ++ map (,p) (Set.elems ns)) ps c
    matchPath' args (p : ps) (PathTree _ cs) = Map.lookup p cs >>= matchPath' args ps

matchSpecialPath ::
  Method ->
  SpecialPath ->
  SpecialPathsMap m ->
  Handler m ->
  (Handler m, Map T.Text T.Text)
matchSpecialPath method sp specialPathsMap defaultH =
  let h = fromMaybe defaultH $ Map.lookup (method, sp) specialPathsMap
   in (h, Map.empty)
