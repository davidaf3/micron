{-# LANGUAGE OverloadedLabels #-}

module Micron.Example.Resource.User.Service
  ( getUser,
    getUsers,
    addUser,
    updateUser,
    deleteUser,
  )
where

import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Text qualified as T
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Database.Selda
  ( Assignment ((:=)),
    MonadIO (liftIO),
    deleteFrom,
    insert,
    literal,
    query,
    restrict,
    select,
    update,
    with,
    (!),
    (.==),
  )
import Micron (BaseError, BaseErrorType (..), Error (..))
import Micron.Example.Crypto (hashPassword)
import Micron.Example.Db (withDb)
import Micron.Example.Resource.User.Filters (UserFilters, applyFilters)
import Micron.Example.Resource.User.Model
  ( User,
    User' (..),
    UserWoID,
    UserWoIDWoPass,
    users,
    withId,
  )

getUser :: Either String T.Text -> IO (Either BaseError User)
getUser (Left _) = return $ Left $ Error InvalidArgument "Invalid user id"
getUser (Right uId) = withDb $ do
  res <- query $ do
    user <- select users
    restrict (user ! #userId .== literal uId)
    return user
  return $ case res of
    [user] -> Right user
    _ -> Left $ Error NotFound "User not found"

getUsers :: Either (Map String String) UserFilters -> IO (Either BaseError [User])
getUsers (Left _) = return $ Left $ Error InvalidArgument "Invalid filters"
getUsers (Right filters) = withDb $ do
  res <- query $ do
    user <- select users
    applyFilters filters user
    return user
  return $ Right res

addUser :: Maybe UserWoID -> IO (Either BaseError User)
addUser Nothing = return $ Left $ Error InvalidArgument "Invalid user"
addUser (Just user) = withDb $ do
  userWithId <- liftIO nextRandom <&> withId user . toText
  hashedPass <- liftIO (hashPassword $ password userWithId)
  _ <- insert users [userWithId {password = hashedPass}]
  return $ Right userWithId

updateUser :: Either String T.Text -> Maybe UserWoIDWoPass -> IO (Either BaseError User)
updateUser (Left _) _ = return $ Left $ Error InvalidArgument "Invalid user id"
updateUser _ Nothing = return $ Left $ Error InvalidArgument "Invalid updated user"
updateUser (Right uId) (Just new) = withDb $ do
  nUpdated <-
    update
      users
      (\user -> user ! #userId .== literal uId)
      (\user -> user `with` [#userName := literal (userName new)])
  return $
    if nUpdated == 0
      then Left $ Error NotFound "User not found"
      else Right $ (withId new uId) {password = T.empty}

deleteUser :: Either String T.Text -> IO (Maybe BaseError)
deleteUser (Left _) = return $ Just $ Error InvalidArgument "Invalid user id"
deleteUser (Right uId) = withDb $ do
  nDeleted <- deleteFrom users (\user -> user ! #userId .== literal uId)
  return $
    if nDeleted == 0
      then Just $ Error NotFound "User not found"
      else Nothing
