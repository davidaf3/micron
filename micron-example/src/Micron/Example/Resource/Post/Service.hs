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
import Micron (BaseError, BaseErrorType (..), Error (Error))
import Micron.Example.Db (withDb)
import Micron.Example.Resource.Post.Model (Post (Post), PostInput (..), posts)
import Micron.Example.Resource.User.Model (User, User' (..))

addPost :: Maybe PostInput -> Maybe User -> IO (Either BaseError Post)
addPost Nothing _ = return $ Left $ Error InvalidArgument "Invalid post data"
addPost _ Nothing = return $ Left $ Error Forbidden "Forbidden"
addPost (Just input) (Just (User {userId})) = do
  post <- nextRandom <&> (\pId -> Post (toText pId) userId (content input))
  _ <- withDb $ do insert posts [post]
  return $ Right post

getPost :: Either String T.Text -> IO (Either BaseError Post)
getPost (Left _) = return $ Left $ Error InvalidArgument "Invalid post id"
getPost (Right pId) = do
  foundPosts <- withDb $ query $ do
    post <- select posts
    restrict (post ! #postId .== literal pId)
    return post
  case foundPosts of
    [post] -> return $ Right post
    _ -> return $ Left $ Error NotFound "Post not found"

getPostsByUser :: Either String T.Text -> IO (Either BaseError [Post])
getPostsByUser (Left _) = return $ Left $ Error InvalidArgument "Invalid user id"
getPostsByUser (Right uId) = do
  userPosts <- withDb $ query $ do
    post <- select posts
    restrict (post ! #userId .== literal uId)
    return post
  return $ Right userPosts

updatePost :: Either String T.Text -> Maybe PostInput -> Maybe User -> IO (Either BaseError Post)
updatePost (Left _) _ _ = return $ Left $ Error InvalidArgument "Invalid post id"
updatePost _ Nothing _ = return $ Left $ Error InvalidArgument "Invalid updated post"
updatePost _ _ Nothing = return $ Left $ Error Forbidden "Forbidden"
updatePost (Right pId) (Just new) (Just (User {userId})) = do
  nUpdated <-
    withDb $
      update
        posts
        (\post -> post ! #postId .== literal pId .&& post ! #userId .== literal userId)
        (\post -> post `with` [#content := literal (content new)])
  return $
    if nUpdated == 0
      then Left $ Error NotFound "Post not found"
      else Right $ Post pId userId (content new)

deletePost :: Either String T.Text -> Maybe User -> IO (Maybe BaseError)
deletePost (Left _) _ = return $ Just $ Error InvalidArgument "Invalid post id"
deletePost _ Nothing = return $ Just $ Error Forbidden "Forbidden"
deletePost (Right pId) (Just (User {userId})) = do
  nDeleted <-
    withDb $
      deleteFrom
        posts
        (\post -> post ! #postId .== literal pId .&& post ! #userId .== literal userId)
  return $
    if nDeleted == 0
      then Just $ Error NotFound "Post not found"
      else Nothing
