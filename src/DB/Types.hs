{-# LANGUAGE DeriveGeneric, OverloadedStrings, QuasiQuotes, RecordWildCards,
             TemplateHaskell, TypeFamilies #-}
module DB.Types
  ( Id(..)
  , fromId
  , Times(..)
  , Post(..)
  , Thread(..)
  , Comment(..)
  , Summary(..)
  , Schema.Author(..)
  , Schema.Tag(..)
  , Schema.Inet(..)
  , Schema.Name
  , Schema.Email
  ) where

import           Data.Aeson.TH
import           Data.ByteString (ByteString)
import           Data.Text       (Text)
import           Data.Time
import           DB.Schema       (Id (..), fromId)
import qualified DB.Schema       as Schema

data Times = Times !UTCTime !(Maybe UTCTime)
  deriving (Eq, Ord, Show)

deriveJSON defaultOptions ''Times

data Post = Post
  { postId       :: (Id Schema.Post)
  , postFeature  :: Bool
  , postImage    :: (Maybe Text)
  , postTitle    :: Text
  , postContent  :: Text
  , postTimes    :: Times
  , postTags     :: [Schema.Tag]
  , postAuthors  :: [Schema.Author]
  , postComments :: Thread
  } deriving (Eq, Ord, Show)

newtype Thread = Thread { commentList :: [Comment] }
  deriving (Eq, Ord, Show)

data Comment = Comment
  { commentId      :: !(Id Schema.Comment)
  , commentAuthor  :: !Schema.Author
  , commentContent :: !Text
  , commentTimes   :: !Times
  } deriving (Eq, Ord, Show)

type Summary = Post

deriveJSON defaultOptions ''Comment
deriveJSON defaultOptions ''Thread
deriveJSON defaultOptions ''Post

