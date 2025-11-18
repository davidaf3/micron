{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (bracket)
import Database.Selda (SeldaT, createTable, insert)
import Database.Selda.Backend (SeldaConnection, runSeldaT, seldaClose)
import Database.Selda.SQLite (SQLite, sqliteOpen)
import Micron.Example.Db (runDbAccess)
import Micron.Example.Resource.User.Filters (UserFilters (UserFilters))
import Micron.Example.Resource.User.Model (User (User), UserView (UserView), users)
import Micron.Example.Resource.User.Service (getUsers)
import Micron.Polysemy.SafeIO (runAsUnsafeIO)
import Polysemy (runM)
import Test.Hspec
  ( SpecWith,
    around,
    beforeWith,
    context,
    describe,
    hspec,
    it,
    shouldBe,
    shouldMatchList,
  )

main :: IO ()
main =
  hspec $
    around (bracket (sqliteOpen ":memory:") seldaClose) $ do
      specGetUsers

specGetUsers :: SpecWith (SeldaConnection SQLite)
specGetUsers = describe "getUsers" $
  beforeWith beforeGetUsers $ do
    context "with empty filters" $
      it "returns all users" $ \conn -> do
        res <- run conn (getUsers (UserFilters Nothing Nothing))
        res `shouldMatchList` [UserView "1" "user1", UserView "2" "user2", UserView "3" "user3"]
    context "with name filter" $ do
      context "when a user matches the name" $
        it "returns that user" $ \conn -> do
          res <- run conn (getUsers (UserFilters (Just "user1") Nothing))
          res `shouldBe` [UserView "1" "user1"]
      context "when no user matches the name" $
        it "returns an empty list" $ \conn -> do
          res <- run conn (getUsers (UserFilters (Just "nil") Nothing))
          res `shouldBe` []
    context "with names filter" $ do
      context "when some users match the names" $
        it "returns those users" $ \conn -> do
          res <- run conn (getUsers (UserFilters Nothing (Just ["user1", "user3", "nil"])))
          res `shouldMatchList` [UserView "1" "user1", UserView "3" "user3"]
      context "when no user matches the names" $
        it "returns an empty list" $ \conn -> do
          res <- run conn (getUsers (UserFilters Nothing (Just ["nil1", "nil2"])))
          res `shouldBe` []
    context "with several filters" $ do
      it "returns users that match all filters" $ \conn -> do
        res <- run conn (getUsers (UserFilters (Just "user1") (Just ["user1", "user3"])))
        res `shouldMatchList` [UserView "1" "user1"]
  where
    beforeGetUsers = setUpDbBeforeTest $ do
      createTable users
      insert
        users
        [ User "1" "pass1" "user1",
          User "2" "pass2" "user2",
          User "3" "pass3" "user3"
        ]
    run conn = runM . runAsUnsafeIO . runDbAccess conn

setUpDbBeforeTest :: SeldaT b IO a -> SeldaConnection b -> IO (SeldaConnection b)
setUpDbBeforeTest setUp conn = runSeldaT setUp conn >> return conn
