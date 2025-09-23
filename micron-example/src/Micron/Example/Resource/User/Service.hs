{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Micron.Example.Resource.User.Service (getUsers, signUp, login) where

import Control.Monad (unless)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Database.Selda (literal, restrict, select, (!), (.==))
import Micron (BaseErrorType (..), Error (..))
import Micron.Example.Crypto (checkPassword, hashPassword)
import Micron.Example.Db (DbAccess, insert, query)
import Micron.Example.Resource.User.Filters (UserFilters, applyFilters)
import Micron.Example.Resource.User.Model (LoginData (..), User (..), UserView (UserView), users)
import Micron.Example.Resource.UserToken.Model (UserToken)
import Micron.Example.Resource.UserToken.Service (addUserToken)
import Micron.Polysemy.SafeIO (SafeIO, performSafeIO)
import Polysemy (Member, Members, Sem)
import Polysemy.Error (throw)
import Polysemy.Error qualified as PE (Error)

getUsers :: (Member DbAccess r) => UserFilters -> Sem r [UserView]
getUsers filters = do
  us <- query $ do
    user <- select users
    applyFilters filters user
    return user
  return $ map (\User {userId, userName} -> UserView userId userName) us

signUp :: (Members '[DbAccess, SafeIO, PE.Error Error] r) => LoginData -> Sem r UserView
signUp LoginData {userName, password} = do
  usersSameName <- query $ do
    userSameName <- select users
    restrict (userSameName ! #userName .== literal userName)
    return userSameName
  unless (null usersSameName) $
    throw (Error InvalidArgument "Username already taken")
  newUserId <- toText <$> performSafeIO nextRandom
  hashedPass <- performSafeIO $ hashPassword password
  _ <- insert users [User newUserId hashedPass userName]
  return $ UserView newUserId userName

login :: (Members '[DbAccess, SafeIO, PE.Error Error] r) => LoginData -> Sem r UserToken
login LoginData {userName, password} = do
  signedUpUsers <- query $ do
    signedUpUser <- select users
    restrict (signedUpUser ! #userName .== literal userName)
    return signedUpUser
  case signedUpUsers of
    [signedUpUser@User {password = signedUpPassword}]
      | checkPassword password signedUpPassword -> addUserToken signedUpUser
    _ -> throw $ Error Unauthorized "Invalid credentials"
