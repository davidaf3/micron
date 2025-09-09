{-# LANGUAGE OverloadedLabels #-}

module Micron.Example.Resource.User.Service (getUsers, signUp, login) where

import Data.Functor ((<&>))
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
import Micron (BaseErrorType (..), Error (..))
import Micron.Example.Crypto (checkPassword, hashPassword)
import Micron.Example.Db (withDb)
import Micron.Example.Resource.User.Filters (UserFilters, applyFilters)
import Micron.Example.Resource.User.Model (LoginData (..), User (..), UserView (..), users)
import Micron.Example.Resource.UserToken.Model (UserToken)
import Micron.Example.Resource.UserToken.Service (addUserToken)

getUsers :: UserFilters -> IO [UserView]
getUsers filters = withDb $ do
  us <- query $ do
    user <- select users
    applyFilters filters user
    return user
  return $ map (\u -> UserView (userId u) (userName u)) us

signUp :: LoginData -> IO (Either Error UserView)
signUp loginData = withDb $ do
  usersSameName <- query $ do
    userSameName <- select users
    restrict (userSameName ! #userName .== literal (loginUserName loginData))
    return userSameName
  if not $ null usersSameName
    then return $ Left $ Error InvalidArgument "Username already taken"
    else do
      newUserId <- liftIO nextRandom <&> toText
      hashedPass <- liftIO (hashPassword $ loginPassword loginData)
      _ <- insert users [User newUserId hashedPass (loginUserName loginData)]
      return $ Right $ UserView newUserId (loginUserName loginData)

login :: LoginData -> IO (Either Error UserToken)
login loginData = withDb $ do
  signedUpUsers <- query $ do
    signedUpUser <- select users
    restrict (signedUpUser ! #userName .== literal (loginUserName loginData))
    return signedUpUser
  case signedUpUsers of
    [signedUpUser]
      | checkPassword (loginPassword loginData) (password $ head signedUpUsers) ->
          liftIO (addUserToken signedUpUser) <&> Right
    _ -> return $ Left $ Error Unauthorized "Invalid credentials"
