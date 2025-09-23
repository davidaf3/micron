{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Micron.Example.Db (DbAccess, query, insert, update, deleteFrom, useSqlite) where

import Data.Data (Proxy (Proxy))
import Data.Kind (Type)
import Database.Selda (Col, Query, Relational, Res, Result, Row, Table)
import Database.Selda qualified as S (deleteFrom, insert, query, update)
import Database.Selda.Backend (SeldaConnection, runSeldaT)
import Database.Selda.SQLite (SQLite, seldaClose, sqliteOpen)
import Micron (Middleware)
import Micron.Polysemy.SafeIO (SafeIO, performSafeIO)
import Polysemy (Member, Members, Sem, interpret, makeSem)
import Polysemy.Resource (Resource, bracket, runResource)

data DbAccessFor b (m :: Type -> Type) a where
  QueryFor :: (Result a) => Query b a -> DbAccessFor b m [Res a]
  InsertFor :: (Relational a) => Proxy b -> Table a -> [a] -> DbAccessFor b m Int
  UpdateFor ::
    (Relational a) =>
    Table a ->
    (Row b a -> Col b Bool) ->
    (Row b a -> Row b a) ->
    DbAccessFor b m Int
  DeleteFromFor :: (Relational a) => Table a -> (Row b a -> Col b Bool) -> DbAccessFor b m Int

makeSem ''DbAccessFor

runDbAccess :: (Members '[SafeIO] r) => SeldaConnection b -> Sem (DbAccessFor b ': r) a -> Sem r a
runDbAccess conn = interpret $ \case
  QueryFor q -> performSafeIO $ runSeldaT (S.query q) conn
  InsertFor _ t i -> performSafeIO $ runSeldaT (S.insert t i) conn
  UpdateFor t f u -> performSafeIO $ runSeldaT (S.update t f u) conn
  DeleteFromFor t f -> performSafeIO $ runSeldaT (S.deleteFrom t f) conn

useDbFor ::
  (Member (SafeIO) r) =>
  IO (SeldaConnection b) ->
  (SeldaConnection b -> IO ()) ->
  Middleware (Sem (DbAccessFor b ': Resource ': r)) (Sem r)
useDbFor open close h req = runResource (bracket alloc dealloc use)
  where
    alloc = performSafeIO open
    dealloc conn = performSafeIO $ close conn
    use conn = runDbAccess conn (h req)

useSqlite ::
  (Member (SafeIO) r) =>
  FilePath ->
  Middleware (Sem (DbAccessFor SQLite ': Resource ': r)) (Sem r)
useSqlite file = useDbFor (sqliteOpen file) seldaClose

type Backend = SQLite

type DbAccess = DbAccessFor Backend

query :: (Member DbAccess r, Result a) => Query Backend a -> Sem r [Res a]
query = queryFor

insert :: (Member DbAccess r, Relational a) => Table a -> [a] -> Sem r Int
insert = insertFor (Proxy :: Proxy Backend)

update ::
  (Member DbAccess r, Relational a) =>
  Table a ->
  (Row Backend a -> Col Backend Bool) ->
  (Row Backend a -> Row Backend a) ->
  Sem r Int
update = updateFor

deleteFrom ::
  (Member DbAccess r, Relational a) =>
  Table a ->
  (Row Backend a -> Col Backend Bool) ->
  Sem r Int
deleteFrom = deleteFromFor
