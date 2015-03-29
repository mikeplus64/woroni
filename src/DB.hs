{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards #-}
module DB
  ( module DB.Types
    -- Queries (as in, no insertions... sorry about the inconsistency :))
  , getComment
  , getComments
  , getSummary
  , getSummariesAround
  , getPost
  , Schema.getTags
    -- Insertions
  , Schema.addAuthor
  , Schema.addComment
  , Schema.addPost
  , Schema.createAuthor
  , Schema.CommentAuthor(..)
  ) where

import Control.Monad.Trans.Maybe

import qualified Data.Text as T

import qualified Database.PostgreSQL.Simple       as SQL
import qualified Database.PostgreSQL.Simple.SqlQQ as SQL

import qualified DB.Query  as Schema
import qualified DB.Schema as Schema
import           DB.Types

getComment :: SQL.Connection -> Id Schema.Comment -> IO (Maybe Comment)
getComment con cid = runMaybeT $ do
  Schema.Comment{..} <- MaybeT $ Schema.getById con cid
  author             <- MaybeT $ Schema.getCommentAuthor con commentId
  return Comment
    { commentAuthor = author
    , commentTimes = Times commentCreated commentUpdated
    , ..
    }

getComments :: SQL.Connection -> Id Schema.Post -> IO Thread
getComments con pid =
  Thread `fmap` SQL.fold con Schema.getComments (SQL.Only pid) []
    (\acc Schema.Comment{..} -> do
      Just author <- Schema.getCommentAuthor con commentId
      return $ Comment
        { commentAuthor = author
        , commentTimes  = Times commentCreated commentUpdated
        , ..
        } : acc)

getSummary :: SQL.Connection -> Id Schema.Post -> Id Schema.Post -> IO (Maybe Summary)
getSummary c sel pid = do
  mpost <- Schema.getById c pid
  case mpost of
    Just post -> do
      tags     <- Schema.getTags c pid
      authors  <- Schema.getAuthors c pid
      return $! Just $! Summary (sel == Schema.postId post) Post
        { postId       = Schema.postId post
        , postContent  = T.take 40 (Schema.postContent post)
        , postTitle    = Schema.postTitle post
        , postTags     = tags
        , postAuthors  = authors
        , postComments = Thread []
        , postTimes    = Times (Schema.postCreated post)
                               (Schema.postUpdated post)
        }
    Nothing -> return Nothing

getSummariesAround :: SQL.Connection -> Id Schema.Post -> IO [Summary]
getSummariesAround c p@(Id pidi) = do
  postIds <- SQL.query c
    [SQL.sql|SELECT post.id FROM post WHERE post.id >= ? AND post.id < ? |]
    (pidi-10, pidi+10)
  catMaybesM (getSummary c p . SQL.fromOnly) postIds

getPost :: SQL.Connection -> Id Schema.Post -> IO (Maybe Post)
getPost c pid = do
  mpost <- Schema.getById c pid
  case mpost of
    Just post -> do
      tags     <- Schema.getTags c pid
      authors  <- Schema.getAuthors c pid
      comments <- getComments c pid
      return (Just Post
        { postId       = Schema.postId post
        , postContent  = Schema.postContent post
        , postTitle    = Schema.postTitle post
        , postTags     = tags
        , postAuthors  = authors
        , postComments = comments
        , postTimes    = Times (Schema.postCreated post)
                               (Schema.postUpdated post)
        })
    Nothing -> return Nothing

--------------------------------------------------------------------------------
-- Util

{-# INLINE catMaybesM #-}
catMaybesM :: (a -> IO (Maybe b)) -> [a] -> IO [b]
catMaybesM f = go []
 where
  go ys (x:xs) = do
    r <- f x
    case r of
      Just y  -> go (y:ys) xs
      Nothing -> go ys xs
  go ys []     = return ys

