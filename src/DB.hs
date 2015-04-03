{-# LANGUAGE GADTs, OverloadedStrings, QuasiQuotes, RankNTypes, RecordWildCards
             #-}
module DB
  ( module DB.Types
    -- Queries (as in, no insertions... sorry about the inconsistency :))
  , getComment
  , getComments
  , getSummary
  , Aggregate(..)
  , getSummaries
  , getPost
  , Schema.getNewestPostId
  , Schema.getTags
  , Schema.getTagNames
  , Schema.getAuthorNames
    -- Insertions
  , Schema.addAuthor
  , Schema.addComment
  , Schema.addPost
  , Schema.createAuthor
  , Schema.CommentAuthor(..)
  ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import qualified Data.Foldable as Foldable
import           Data.Monoid
import qualified Data.Text     as T

import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import Snap.Snaplet.PostgresqlSimple

import qualified DB.Query  as Schema
import qualified DB.Schema as Schema
import           DB.Types

getComment :: HasPostgres m => Id Schema.Comment -> m (Maybe Comment)
getComment cid = runMaybeT $ do
  Schema.Comment{..} <- MaybeT $ Schema.getById cid
  author             <- MaybeT $ Schema.getCommentAuthor commentId
  return Comment
    { commentAuthor = author
    , commentTimes = Times commentCreated commentUpdated
    , ..
    }

getComments :: HasPostgres m => Id Schema.Post -> m Thread
getComments pid = do
  pg <- getPostgresState
  Thread `liftM` fold Schema.getComments (Only pid) []
    (\acc Schema.Comment{..} -> do
      Just author <- runReaderT (Schema.getCommentAuthor commentId) pg
      return $ Comment
        { commentAuthor = author
        , commentTimes  = Times commentCreated commentUpdated
        , ..
        } : acc)

getSummary :: HasPostgres m => Int -> Id Schema.Post -> m (Maybe Summary)
getSummary size pid = do
  mpost <- Schema.getById pid
  case mpost of
    Just Schema.Post{..} -> do
      tags     <- Schema.getTags pid
      authors  <- Schema.getAuthors pid
      return (Just Post
        { postId       = postId
        , postFeature  = postFeature
        , postImage    = postImage
        , postTitle    = postTitle
        , postContent  = takeIsh size postContent
        , postTimes    = Times postCreated postUpdated
        , postTags     = tags
        , postAuthors  = authors
        , postComments = Thread []
        })
    Nothing -> return Nothing

data Aggregate a = Aggregate
  { aggLimit   :: !Int
  , aggSize    :: !Int
  , aggTop     :: !(Maybe (Id a))
  , aggTags    :: !(Maybe [Id Tag])
  , aggAuthors :: !(Maybe [Id Author])
  , aggTerms   :: !(Maybe [T.Text])
  } deriving Show

getSummaries :: HasPostgres m => Aggregate Schema.Post -> m [Summary]
getSummaries Aggregate{..} = do
  let params =
        (Foldable.concatMap (one . toField . In) aggTags) ++
        (Foldable.concatMap (one . toField . In) aggAuthors) ++
        (Foldable.concatMap (one . toField) aggTop) ++
        [toField aggLimit]

  postIds <- query getQuery params

  catMaybesM
    (\pid -> getSummary aggSize (fromOnly pid))
    postIds

 where
  one = (:[])
  joinTags _tags =
    "JOIN post_to_tag ON post.id = post_to_tag.post AND post_to_tag.tag IN ? \
    \JOIN tag         ON tag.id  = post_to_tag.tag "

  joinAuthors _authors =
    "JOIN post_to_author ON post.id = post_to_author.post \
    \  AND post_to_author.author IN ? \
    \JOIN author ON author.id = post_to_author.author "

  wherePid _pid =
    "WHERE post.id > ? - 3 \n"

  getQuery :: Query
  getQuery =
    "SELECT DISTINCT post.id FROM post " <>
    Foldable.foldMap joinTags aggTags <>
    Foldable.foldMap joinAuthors aggAuthors <>
    Foldable.foldMap wherePid aggTop <>
    "ORDER BY post.id DESC " <>
    "LIMIT ?"


takeIsh :: Int -> T.Text -> T.Text
takeIsh n' str = loop 0
 where
  n = min n' (T.length str)
  loop i
    | i < n                = loop (i + 1)
    | i >= T.length str    = str
    | T.index str i == ' ' = T.take i str `T.append` "..."
    | otherwise            = loop (i+1)

getPost :: HasPostgres m => Id Schema.Post -> m (Maybe Post)
getPost pid = do
  mpost <- Schema.getById pid
  case mpost of
    Just Schema.Post{..} -> do
      tags     <- Schema.getTags pid
      authors  <- Schema.getAuthors pid
      comments <- getComments pid
      return $ Just Post
        { postTags     = tags
        , postAuthors  = authors
        , postComments = comments
        , postTimes    = Times postCreated postUpdated
        , ..
        }
    Nothing -> return Nothing

--------------------------------------------------------------------------------
-- Util

{-# INLINE catMaybesM #-}
catMaybesM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
catMaybesM f = go []
 where
  go ys (x:xs) = do
    r <- f x
    case r of
      Just y  -> go (y:ys) xs
      Nothing -> go ys xs
  go ys []     = return ys

