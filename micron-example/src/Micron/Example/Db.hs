module Micron.Example.Db (withDb) where

import Control.Monad.IO.Class (MonadIO)
import Database.Selda
  ( MonadMask,
    SeldaT,
  )
import Database.Selda.SQLite (SQLite, withSQLite)

withDb :: (MonadIO m, MonadMask m) => SeldaT SQLite m a -> m a
withDb = withSQLite "data/database.db"
