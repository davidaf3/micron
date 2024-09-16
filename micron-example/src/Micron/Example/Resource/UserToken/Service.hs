{-# LANGUAGE OverloadedLabels #-}

module Micron.Example.Resource.UserToken.Service
  ( addUserToken,
    getUserByToken,
  )
where

import Data.Functor ((<&>))
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Database.Selda
  ( MonadIO (liftIO),
    insert,
    literal,
    query,
    restrict,
    select,
    (!),
    (.==),
  )
import Micron.Example.Db (withDb)
import Micron.Example.Resource.User.Model (User, User' (..), users)
import Micron.Example.Resource.UserToken.Model (UserToken (UserToken), userTokens)

addUserToken :: User -> IO UserToken
addUserToken user = withDb $ do
  token <- liftIO nextRandom <&> toText
  let userToken = UserToken token (userId user)
  _ <- insert userTokens [userToken]
  return userToken

getUserByToken :: T.Text -> IO (Maybe User)
getUserByToken token = withDb $ do
  matchingUsers <- query $ do
    user <- select users
    userToken <- select userTokens
    restrict (userToken ! #token .== literal token)
    restrict (userToken ! #userId .== user ! #userId)
    return user
  return $ listToMaybe matchingUsers
