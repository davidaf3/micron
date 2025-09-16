{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module Micron.Example.Resource.Post.Service
  ( addPost,
    getPost,
    getPostsByUser,
    updatePost,
    deletePost,
  )
where

import Control.Monad (when)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor ((<&>))
import Data.Text qualified as T
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Database.Selda
  ( Assignment ((:=)),
    MonadMask,
    deleteFrom,
    insert,
    literal,
    query,
    restrict,
    select,
    update,
    with,
    (!),
    (.&&),
    (.==),
  )
import Micron (BaseErrorType (..), Error (Error))
import Micron.Example.Db (withDb)
import Micron.Example.Resource.Post.Model (Post (Post), PostInput (..), posts)
import Micron.Example.Resource.User.Model (User (..))

addPost :: (MonadIO m, MonadMask m) => PostInput -> User -> m Post
addPost (PostInput {content}) (User {userId}) = do
  post <- liftIO $ nextRandom <&> (\pId -> Post (toText pId) userId content)
  _ <- withDb $ do insert posts [post]
  return post

getPost :: (MonadMask m, MonadIO m, MonadError Error m) => T.Text -> m Post
getPost pId = do
  foundPosts <- withDb $ query $ do
    post <- select posts
    restrict (post ! #postId .== literal pId)
    return post
  case foundPosts of
    [post] -> return post
    _ -> throwError $ Error NotFound "Post not found"

getPostsByUser :: (MonadMask m, MonadIO m) => T.Text -> m [Post]
getPostsByUser uId = do
  withDb $ query $ do
    post <- select posts
    restrict (post ! #userId .== literal uId)
    return post

updatePost :: (MonadIO m, MonadMask m, MonadError Error m) => T.Text -> PostInput -> User -> m Post
updatePost pId (PostInput {content = newContent}) (User {userId}) = do
  nUpdated <-
    withDb $
      update
        posts
        (\post -> post ! #postId .== literal pId .&& post ! #userId .== literal userId)
        (\post -> post `with` [#content := literal newContent])
  when (nUpdated == 0) $
    throwError (Error NotFound "Post not found")
  return $ Post pId userId newContent

deletePost :: (MonadIO m, MonadMask m, MonadError Error m) => T.Text -> User -> m ()
deletePost pId (User {userId}) = do
  nDeleted <-
    withDb $
      deleteFrom
        posts
        (\post -> post ! #postId .== literal pId .&& post ! #userId .== literal userId)
  when (nDeleted == 0) $
    throwError (Error NotFound "Post not found")
