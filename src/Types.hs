{-# LANGUAGE DeriveGeneric, OverloadedStrings, QuasiQuotes, RecordWildCards,
             TemplateHaskell, TypeFamilies #-}
{-# OPTIONS_GHC -ddump-splices #-}
module Types (Id(..), Schema.fromId, Times(..), LPost(..), LComment(..)
             , Schema.Author(..)
             , Schema.Tag(..)
             , Summary(..)
             , getComment, getComments, getPost, getSummariesAround) where
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Text.Lazy.Encoding
import           Data.Time
import           Data.Vector             (Vector, fromList)
import           GHC.Generics

import Control.Lens
import Control.Monad.Trans.Maybe

import qualified Database.PostgreSQL.Simple       as SQL
import qualified Database.PostgreSQL.Simple.SqlQQ as SQL

import           Schema (Author, Comment, Id (..), Post, Tag)
import qualified Schema

data Times = Times !UTCTime !(Maybe UTCTime)
  deriving (Eq, Ord, Show)

deriveJSON defaultOptions ''Times

data LPost = LPost
  { postId       :: !(Id Post)
  , postContent  :: !Text
  , postTitle    :: !Text
  , postTimes    :: !Times
  , postTags     :: ![Tag]
  , postAuthors  :: ![Author]
  , postComments :: ![LComment]
  } deriving (Eq, Ord, Show)

data LComment = LComment
  { commentId      :: !(Id Comment)
  , commentAuthor  :: !Author
  , commentContent :: !Text
  , commentTimes   :: !Times
  } deriving (Eq, Ord, Show)

data Summary = Summary
  { summarySelected :: !Bool
  , summaryPost     :: LPost -- postComments should be []
  }

deriveJSON defaultOptions ''LComment
deriveJSON defaultOptions ''LPost

--------------------------------------------------------------------------------
-- getting these shinier types from the database into haskell land

getComment :: SQL.Connection -> Id Comment -> IO (Maybe LComment)
getComment con cid = runMaybeT $ do
  Schema.Comment{..} <- MaybeT $ Schema.getById con cid
  author             <- MaybeT $ Schema.getCommentAuthor con commentId
  return LComment
    { commentAuthor = author
    , commentTimes = Times commentCreated commentUpdated
    , ..
    }

getComments :: SQL.Connection -> Id Post -> IO [LComment]
getComments con pid = do
  SQL.fold con Schema.getComments (SQL.Only pid) [] $
    \acc Schema.Comment{..} -> do
      Just author <- Schema.getCommentAuthor con commentId
      return $ LComment
        { commentAuthor = author
        , commentTimes  = Times commentCreated commentUpdated
        , ..
        } : acc

getSummary :: SQL.Connection -> Id Post -> IO (Maybe Summary)
getSummary c pid = do
  mpost <- Schema.getById c pid
  case mpost of
    Just post -> do
      tags     <- Schema.getTags c pid
      authors  <- Schema.getAuthors c pid
      return $! Just $! Summary (pid == Schema.postId post) LPost
        { postId       = Schema.postId post
        , postContent  = T.take 40 (Schema.postContent post)
        , postTitle    = Schema.postTitle post
        , postTags     = tags
        , postAuthors  = authors
        , postComments = []
        , postTimes    = Times (Schema.postCreated post)
                               (Schema.postUpdated post)
        }
    Nothing -> return Nothing

getSummariesAround :: SQL.Connection -> Id Post -> IO [Summary]
getSummariesAround c pid@(Id pidi) =
  SQL.fold c
    [SQL.sql|SELECT * FROM post WHERE post.id >= ? AND post.id < ? |]
    (pidi-10, pidi+10)
    []
    -- since we don't care about tags here... don't bother
    (\acc post -> do
        authors <- Schema.getAuthors c pid
        tags    <- Schema.getTags    c pid
        return $! Summary (pid == Schema.postId post) LPost
          { postId       = Schema.postId post
          , postContent  = T.take 320 (Schema.postContent post)
          , postTitle    = Schema.postTitle post
          , postTags     = tags
          , postAuthors  = authors
          , postComments = []
          , postTimes    = Times
                           (Schema.postCreated post)
                           (Schema.postUpdated post)
          } : acc)

getPost :: SQL.Connection -> Id Post -> IO (Maybe (LPost))
getPost c pid = do
  mpost <- Schema.getById c pid
  case mpost of
    Just post -> do
      tags     <- Schema.getTags c pid
      authors  <- Schema.getAuthors c pid
      comments <- getComments c pid
      return $! Just LPost
        { postId       = Schema.postId post
        , postContent  = Schema.postContent post
        , postTitle    = Schema.postTitle post
        , postTags     = tags
        , postAuthors  = authors
        , postComments = comments
        , postTimes    = Times (Schema.postCreated post)
                               (Schema.postUpdated post)
        }
    Nothing -> return Nothing


