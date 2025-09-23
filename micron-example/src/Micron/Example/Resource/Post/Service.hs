{-# LANGUAGE NamedFieldPuns #-}

module Micron.Example.Resource.Post.Service
  ( addPost,
    getPost,
    getPostsByUser,
    updatePost,
    deletePost,
  )
where

import Control.Monad (when)
import Data.Functor ((<&>))
import Data.Text qualified as T
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Database.Selda (Assignment ((:=)), literal, restrict, select, with, (!), (.&&), (.==))
import Micron (BaseErrorType (..), Error (Error))
import Micron.Example.Db (DbAccess, deleteFrom, insert, query, update)
import Micron.Example.Resource.Post.Model (Post (Post), PostInput (..), posts)
import Micron.Example.Resource.User.Model (User (..))
import Micron.Polysemy.SafeIO (SafeIO, performSafeIO)
import Polysemy (Member, Members, Sem)
import Polysemy.Error (throw)
import Polysemy.Error qualified as PE (Error)

addPost :: (Members '[DbAccess, SafeIO] r) => PostInput -> User -> Sem r Post
addPost (PostInput {content}) (User {userId}) = do
  post <- performSafeIO $ nextRandom <&> (\pId -> Post (toText pId) userId content)
  _ <- insert posts [post]
  return post

getPost :: (Members '[DbAccess, PE.Error Error] r) => T.Text -> Sem r Post
getPost pId = do
  foundPosts <- query $ do
    post <- select posts
    restrict (post ! #postId .== literal pId)
    return post
  case foundPosts of
    [post] -> return post
    _ -> throw $ Error NotFound "Post not found"

getPostsByUser :: (Member (DbAccess) r) => T.Text -> Sem r [Post]
getPostsByUser uId = query $ do
  post <- select posts
  restrict (post ! #userId .== literal uId)
  return post

updatePost :: (Members '[DbAccess, PE.Error Error] r) => T.Text -> PostInput -> User -> Sem r Post
updatePost pId (PostInput {content = newContent}) (User {userId}) = do
  nUpdated <-
    update
      posts
      (\post -> post ! #postId .== literal pId .&& post ! #userId .== literal userId)
      (\post -> post `with` [#content := literal newContent])
  when (nUpdated == 0) $
    throw (Error NotFound "Post not found")
  return $ Post pId userId newContent

deletePost :: (Members '[DbAccess, PE.Error Error] r) => T.Text -> User -> Sem r ()
deletePost pId (User {userId}) = do
  nDeleted <-
    deleteFrom
      posts
      (\post -> post ! #postId .== literal pId .&& post ! #userId .== literal userId)
  when (nDeleted == 0) $
    throw (Error NotFound "Post not found")
