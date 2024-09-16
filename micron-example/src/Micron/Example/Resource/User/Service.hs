{-# LANGUAGE OverloadedLabels #-}

module Micron.Example.Resource.User.Service
  ( getUsers,
    signUp,
    login,
  )
where

import Data.Functor ((<&>))
import Data.Map (Map)
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
import Micron (BaseError, BaseErrorType (..), Error (..))
import Micron.Example.Crypto (checkPassword, hashPassword)
import Micron.Example.Db (withDb)
import Micron.Example.Resource.User.Filters (UserFilters, applyFilters)
import Micron.Example.Resource.User.Model
  ( LoginData,
    User,
    User' (..),
    users,
  )
import Micron.Example.Resource.UserToken.Model (UserToken)
import Micron.Example.Resource.UserToken.Service (addUserToken)

getUsers :: Either (Map String String) UserFilters -> IO (Either BaseError [User])
getUsers (Left _) = return $ Left $ Error InvalidArgument "Invalid filters"
getUsers (Right filters) = withDb $ do
  res <- query $ do
    user <- select users
    applyFilters filters user
    return user
  return $ Right res

signUp :: Maybe LoginData -> IO (Either BaseError User)
signUp Nothing = return $ Left $ Error InvalidArgument "Invalid login data"
signUp (Just user) = withDb $ do
  usersSameName <- query $ do
    userSameName <- select users
    restrict (userSameName ! #userName .== literal (userName user))
    return userSameName
  if not $ null usersSameName
    then return $ Left $ Error InvalidArgument "Username already taken"
    else do
      userWithId <- liftIO nextRandom <&> (\uId -> user {userId = toText uId})
      hashedPass <- liftIO (hashPassword $ password userWithId)
      _ <- insert users [userWithId {password = hashedPass}]
      return $ Right userWithId

login :: Maybe LoginData -> IO (Either BaseError UserToken)
login Nothing = return $ Left $ Error InvalidArgument "Invalid credentials"
login (Just user) = withDb $ do
  signedUpUsers <- query $ do
    signedUpUser <- select users
    restrict (signedUpUser ! #userName .== literal (userName user))
    return signedUpUser
  case signedUpUsers of
    [signedUpUser]
      | checkPassword (password user) (password $ head signedUpUsers) ->
          liftIO (addUserToken signedUpUser) <&> Right
    _ -> return $ Left $ Error Unauthorized "Invalid credentials"
