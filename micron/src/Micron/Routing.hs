{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Micron.Routing
  ( mkRoute,
    get,
    post,
    put,
    delete,
    patch,
    options,
    head_,
    connect,
    trace,
    allMethods,
    mkPaths,
    matchPath,
    matchSpecialPath,
    Paths (..),
    SpecialPath (..),
    Path (..),
    R,
    Handler,
    Route (..),
    Routes,
    type (./),
    type (/),
    type (/:),
  )
where

import Control.Monad.State (State, modify)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Singletons (SingI (sing), demote, fromSing)
import Data.Text qualified as T
import Micron.Request (Request)
import Micron.Sing.Path
  ( DPathPart (..),
    DSpecialPath,
    IntoPath,
    Path (..),
    R,
    SDPath (..),
    SpecialPath (..),
    type (./),
    type (/),
    type (/:),
  )
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
import Prelude hiding ((/))

type Handler p m = Request p -> m Wai.Response

data Route m where
  Route :: Method -> (SDPath p) -> (Handler p m) -> Route m

type Routes m = State [Route m] ()

mkRoute :: forall a m. (SingI (IntoPath a)) => Method -> Handler (IntoPath a) m -> Routes m
mkRoute method handler = modify (Route method (sing @(IntoPath a)) handler :)

get :: forall a m. (SingI (IntoPath a)) => Handler (IntoPath a) m -> Routes m
get = mkRoute @a methodGet

post :: forall a m. (SingI (IntoPath a)) => Handler (IntoPath a) m -> Routes m
post = mkRoute @a methodPost

put :: forall a m. (SingI (IntoPath a)) => Handler (IntoPath a) m -> Routes m
put = mkRoute @a methodPut

delete :: forall a m. (SingI (IntoPath a)) => Handler (IntoPath a) m -> Routes m
delete = mkRoute @a methodDelete

patch :: forall a m. (SingI (IntoPath a)) => Handler (IntoPath a) m -> Routes m
patch = mkRoute @a methodPatch

options :: forall a m. (SingI (IntoPath a)) => Handler (IntoPath a) m -> Routes m
options = mkRoute @a methodOptions

head_ :: forall a m. (SingI (IntoPath a)) => Handler (IntoPath a) m -> Routes m
head_ = mkRoute @a methodHead

connect :: forall a m. (SingI (IntoPath a)) => Handler (IntoPath a) m -> Routes m
connect = mkRoute @a methodConnect

trace :: forall a m. (SingI (IntoPath a)) => Handler (IntoPath a) m -> Routes m
trace = mkRoute @a methodTrace

allMethods :: [Method]
allMethods =
  [ methodGet,
    methodPost,
    methodPut,
    methodDelete,
    methodPatch,
    methodOptions,
    methodHead,
    methodConnect,
    methodTrace
  ]

data Paths m
  = PathTree (Map.Map Method (Route m)) (Map.Map T.Text (Paths m))
  | PathParam (Map.Map Method (Route m)) (Set T.Text) (Paths m)

type SpecialPaths m = Map.Map (Method, DSpecialPath) (Route m)

emptyPaths :: Paths m
emptyPaths = PathTree Map.empty Map.empty

mkPaths :: [Route m] -> (Paths m, SpecialPaths m)
mkPaths = foldl insertRoute (emptyPaths, Map.empty)
  where
    insertRoute :: (Paths m, SpecialPaths m) -> Route m -> (Paths m, SpecialPaths m)
    insertRoute (ps, sps) r@(Route m (SDSpecial sp) _) = (ps, Map.insert (m, fromSing sp) r sps)
    insertRoute (ps, sps) r@(Route _ (SDParts parts) _) = (addRoute (fromSing parts) r ps, sps)

addRoute :: [DPathPart] -> Route m -> Paths m -> Paths m
addRoute [] r@(Route m _ _) (PathParam rs n c) = PathParam (Map.insert m r rs) n c
addRoute [] r@(Route m _ _) (PathTree rs cs) = PathTree (Map.insert m r rs) cs
addRoute (DParam x : xs) r (PathParam rs ns c) =
  PathParam rs (Set.insert x ns) (addRoute xs r c)
addRoute (DExact x : xs) r (PathParam rs _ _) =
  PathTree rs (Map.singleton x (addRoute xs r emptyPaths))
addRoute (DParam x : xs) r (PathTree rs _) =
  PathParam rs (Set.singleton x) (addRoute xs r emptyPaths)
addRoute (DExact x : xs) r (PathTree rs cs) =
  PathTree rs (Map.insertWith (const $ addRoute xs r) x (addRoute xs r emptyPaths) cs)

matchPath :: Method -> [T.Text] -> Paths m -> Maybe (Route m, Map.Map T.Text T.Text)
matchPath method = go Map.empty
  where
    go args [] (PathParam rs _ _) = (,args) <$> Map.lookup method rs
    go args [] (PathTree rs _) = (,args) <$> Map.lookup method rs
    go args (p : ps) (PathParam _ ns c) = go (foldr (`Map.insert` p) args (Set.elems ns)) ps c
    go args (p : ps) (PathTree _ cs) = Map.lookup p cs >>= go args ps

matchSpecialPath ::
  forall sp m.
  (SingI sp) =>
  Method ->
  SpecialPaths m ->
  Handler ('Special sp) m ->
  (Route m, Map.Map T.Text T.Text)
matchSpecialPath method specialPathsMap defaultH =
  let maybeR = Map.lookup (method, demote @sp) specialPathsMap
      defaultR = Route method (sing @('Special sp)) defaultH
   in (fromMaybe defaultR maybeR, Map.empty)
