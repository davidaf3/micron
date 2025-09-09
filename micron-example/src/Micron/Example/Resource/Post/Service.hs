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

addPost :: PostInput -> Maybe User -> IO (Either Error Post)
addPost _ Nothing = return $ Left $ Error Forbidden "Forbidden"
addPost input (Just (User {userId})) = do
  post <- nextRandom <&> (\pId -> Post (toText pId) userId (content input))
  _ <- withDb $ do insert posts [post]
  return $ Right post

getPost :: T.Text -> IO (Either Error Post)
getPost pId = do
  foundPosts <- withDb $ query $ do
    post <- select posts
    restrict (post ! #postId .== literal pId)
    return post
  case foundPosts of
    [post] -> return $ Right post
    _ -> return $ Left $ Error NotFound "Post not found"

getPostsByUser :: T.Text -> IO [Post]
getPostsByUser uId = do
  withDb $ query $ do
    post <- select posts
    restrict (post ! #userId .== literal uId)
    return post

updatePost :: T.Text -> PostInput -> Maybe User -> IO (Either Error Post)
updatePost _ _ Nothing = return $ Left $ Error Forbidden "Forbidden"
updatePost pId updated (Just (User {userId})) = do
  nUpdated <-
    withDb $
      update
        posts
        (\post -> post ! #postId .== literal pId .&& post ! #userId .== literal userId)
        (\post -> post `with` [#content := literal (content updated)])
  return $
    if nUpdated == 0
      then Left $ Error NotFound "Post not found"
      else Right $ Post pId userId (content updated)

deletePost :: T.Text -> Maybe User -> IO (Either Error ())
deletePost _ Nothing = return $ Left $ Error Forbidden "Forbidden"
deletePost pId (Just (User {userId})) = do
  nDeleted <-
    withDb $
      deleteFrom
        posts
        (\post -> post ! #postId .== literal pId .&& post ! #userId .== literal userId)
  return $
    if nDeleted == 0
      then Left $ Error NotFound "Post not found"
      else Right ()
