{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module Micron.Example.Resource.User.Service (getUsers, signUp, login) where

import Control.Monad (unless)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Database.Selda
  ( MonadMask,
    insert,
    literal,
    query,
    restrict,
    select,
    (!),
    (.==),
  )
import Micron (BaseErrorType (..), Error (..))
import Micron.Example.Crypto (checkPassword, hashPassword)
import Micron.Example.Db (withDb)
import Micron.Example.Resource.User.Filters (UserFilters, applyFilters)
import Micron.Example.Resource.User.Model (LoginData (..), User (..), UserView (UserView), users)
import Micron.Example.Resource.UserToken.Model (UserToken)
import Micron.Example.Resource.UserToken.Service (addUserToken)

getUsers :: (MonadIO m, MonadMask m) => UserFilters -> m [UserView]
getUsers filters = withDb $ do
  us <- query $ do
    user <- select users
    applyFilters filters user
    return user
  return $ map (\User {userId, userName} -> UserView userId userName) us

signUp :: (MonadIO m, MonadMask m, MonadError Error m) => LoginData -> m UserView
signUp LoginData {userName, password} = do
  usersSameName <- withDb $ query $ do
    userSameName <- select users
    restrict (userSameName ! #userName .== literal userName)
    return userSameName
  unless (null usersSameName) $
    throwError (Error InvalidArgument "Username already taken")
  newUserId <- toText <$> liftIO nextRandom
  hashedPass <- liftIO $ hashPassword password
  _ <- withDb $ insert users [User newUserId hashedPass userName]
  return $ UserView newUserId userName

login :: (MonadIO m, MonadMask m, MonadError Error m) => LoginData -> m UserToken
login LoginData {userName, password} = do
  signedUpUsers <- withDb $ query $ do
    signedUpUser <- select users
    restrict (signedUpUser ! #userName .== literal userName)
    return signedUpUser
  case signedUpUsers of
    [signedUpUser@User {password = signedUpPassword}]
      | checkPassword password signedUpPassword -> addUserToken signedUpUser
    _ -> throwError $ Error Unauthorized "Invalid credentials"
