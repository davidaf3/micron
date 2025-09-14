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
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.Text qualified as T
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Database.Selda
  ( Assignment ((:=)),
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
import Micron.Example.Utils (AppM)

addPost :: PostInput -> User -> AppM Post
addPost (PostInput {content}) (User {userId}) = do
  post <- liftIO $ nextRandom <&> (\pId -> Post (toText pId) userId content)
  _ <- withDb $ do insert posts [post]
  return post

getPost :: T.Text -> AppM Post
getPost pId = do
  foundPosts <- withDb $ query $ do
    post <- select posts
    restrict (post ! #postId .== literal pId)
    return post
  case foundPosts of
    [post] -> return post
    _ -> throwError $ Error NotFound "Post not found"

getPostsByUser :: T.Text -> AppM [Post]
getPostsByUser uId = do
  withDb $ query $ do
    post <- select posts
    restrict (post ! #userId .== literal uId)
    return post

updatePost :: T.Text -> PostInput -> User -> AppM Post
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

deletePost :: T.Text -> User -> AppM ()
deletePost pId (User {userId}) = do
  nDeleted <-
    withDb $
      deleteFrom
        posts
        (\post -> post ! #postId .== literal pId .&& post ! #userId .== literal userId)
  when (nDeleted == 0) $
    throwError (Error NotFound "Post not found")
