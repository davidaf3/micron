module Micron.Example.Resource.UserToken.Service (addUserToken, getUserByToken) where

import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Database.Selda (literal, restrict, select, (!), (.==))
import Micron.Example.Db (DbAccess, insert, query)
import Micron.Example.Resource.User.Model (User (..), users)
import Micron.Example.Resource.UserToken.Model (UserToken (UserToken), userTokens)
import Micron.Polysemy.SafeIO (SafeIO, performSafeIO)
import Polysemy (Member, Members, Sem)

addUserToken :: (Members '[DbAccess, SafeIO] r) => User -> Sem r UserToken
addUserToken user = do
  token <- toText <$> performSafeIO nextRandom
  let userToken = UserToken token (userId user)
  _ <- insert userTokens [userToken]
  return userToken

getUserByToken :: (Member (DbAccess) r) => T.Text -> Sem r (Maybe User)
getUserByToken token = do
  matchingUsers <- query $ do
    user <- select users
    userToken <- select userTokens
    restrict (userToken ! #token .== literal token)
    restrict (userToken ! #userId .== user ! #userId)
    return user
  return $ listToMaybe matchingUsers
