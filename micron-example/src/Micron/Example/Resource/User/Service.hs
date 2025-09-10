{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module Micron.Example.Resource.User.Service (getUsers, signUp, login) where

import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Database.Selda
  ( insert,
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

getUsers :: UserFilters -> IO [UserView]
getUsers filters = withDb $ do
  us <- query $ do
    user <- select users
    applyFilters filters user
    return user
  return $ map (\User {userId, userName} -> UserView userId userName) us

signUp :: LoginData -> IO (Either Error UserView)
signUp LoginData {userName, password} = do
  usersSameName <- withDb $ query $ do
    userSameName <- select users
    restrict (userSameName ! #userName .== literal userName)
    return userSameName
  if not $ null usersSameName
    then return $ Left $ Error InvalidArgument "Username already taken"
    else do
      newUserId <- toText <$> nextRandom
      hashedPass <- hashPassword password
      _ <- withDb $ insert users [User newUserId hashedPass userName]
      return $ Right $ UserView newUserId userName

login :: LoginData -> IO (Either Error UserToken)
login LoginData {userName, password} = do
  signedUpUsers <- withDb $ query $ do
    signedUpUser <- select users
    restrict (signedUpUser ! #userName .== literal userName)
    return signedUpUser
  case signedUpUsers of
    [signedUpUser@User {password = signedUpPassword}]
      | checkPassword password signedUpPassword ->
          Right <$> addUserToken signedUpUser
    _ -> return $ Left $ Error Unauthorized "Invalid credentials"
