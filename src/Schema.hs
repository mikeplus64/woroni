{-# LANGUAGE DataKinds, DeriveDataTypeable, GADTs, NamedFieldPuns,
             OverloadedStrings, PolyKinds, QuasiQuotes, RecordWildCards,
             TemplateHaskell, TypeFamilies #-}
module Schema where
import Control.Lens

import Data.Maybe (listToMaybe)

import           Data.Text (Text)
import qualified Data.Text as T

import Data.Time
import Data.Typeable
import Data.Vector   (Vector, fromList)
import Data.Word     (Word)

import Data.ByteString                            (ByteString)
import Database.PostgreSQL.Query.TH               (deriveFromRow, deriveToRow)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.TypeInfo.Static

type Name  = Text
type Email = Text

newtype Id a = Id Int deriving (Eq, Show, Ord)
instance FromField (Id a) where fromField f mbs = fmap Id (fromField f mbs)
instance ToField   (Id a) where toField (Id a) = toField a

data Post = Post
  { postId      :: !(Id Post)
  , postTitle   :: !Text
  , postContent :: !Text
  , postCreated :: !UTCTime
  , postUpdated :: !(Maybe UTCTime)
  } deriving (Eq, Ord, Show)

-- | Same as a post - but this has the authors, comments and tags as well
data Article = Article
  { articlePost     :: !Post
  , articleTags     :: !(Vector Tag)
  , articleAuthors  :: !(Vector Author)
  , articleComments :: !(Vector Comment)
  } deriving (Eq, Ord, Show)

data Author = Author
  { authorId    :: !(Id Author)
  , authorAddr  :: !Inet
  , authorName  :: !Text
  , authorEmail :: !(Maybe Text)
  } deriving (Eq, Ord, Show)

data Comment = Comment
  { commentId      :: !(Id Comment)
  , commentParent  :: !(Id Post)
  , commentAuthor  :: !(Id Author)
  , commentContent :: !Text
  , commentCreated :: !UTCTime
  , commentUpdated :: !(Maybe UTCTime)
  } deriving (Eq, Ord, Show)

data Tag    = Tag !(Id Tag) !Text            deriving (Eq, Ord, Show)
data Wrote  = Wrote  !(Id Post) !(Id Author) deriving (Eq, Ord, Show)
data Tagged = Tagged !(Id Post) !(Id Tag)    deriving (Eq, Ord, Show)

newtype Inet = Inet ByteString deriving (Show, Eq, Ord, Typeable)

instance FromField Inet where
  fromField field mbs =
    if typeOid field /= typoid inet
    then returnError Incompatible field ""
    else
      case mbs of
        Nothing -> returnError UnexpectedNull field ""
        Just bs -> return (Inet bs)

instance ToField Inet where
  toField (Inet bs) = Escape bs

deriveFromRow ''Post    ; deriveToRow ''Post
deriveFromRow ''Author  ; deriveToRow ''Author
deriveFromRow ''Comment ; deriveToRow ''Comment
deriveFromRow ''Tag     ; deriveToRow ''Tag
deriveFromRow ''Tagged  ; deriveToRow ''Tagged
deriveFromRow ''Wrote   ; deriveToRow ''Wrote

class HasId a where
  getById :: Connection -> Id a -> IO (Maybe a)

instance HasId Author where
  getById conn id' = listToMaybe `fmap` query conn
    "SELECT * FROM author WHERE author.id=? LIMIT 1;"
    (Only id')

instance HasId Post where
  getById conn id' = listToMaybe `fmap` query conn
    "SELECT * FROM post WHERE post.id=? LIMIT 1;"
    (Only id')

instance HasId Comment where
  getById conn id' = listToMaybe `fmap` query conn
    "SELECT * FROM comment WHERE comment.id=? LIMIT 1;"
    (Only id')

instance HasId Tag where
  getById conn id' = listToMaybe `fmap` query conn
    "SELECT * FROM tag WHERE tag.id=? LIMIT 1;"
    (Only id')

--------------------------------------------------------------------------------
-- Post queries

getArticle :: Connection -> Id Post -> IO (Maybe Article)
getArticle c pid = do
  mpost <- getById c pid
  case mpost of
    Just post -> do
      tags     <- getTags c pid
      authors  <- getAuthors c pid
      comments <- getComments c pid

      return $! Just Article
        { articlePost     = post
        , articleTags     = fromList tags
        , articleAuthors  = fromList authors
        , articleComments = fromList comments
        }
    Nothing   -> return Nothing

getComments :: Connection -> Id Post -> IO [Comment]
getComments c =
  query c [sql|SELECT comment.* FROM comment WHERE comment.parent=?|] . Only

getTags :: Connection -> Id Post -> IO [Tag]
getTags c =
  query c
    [sql|SELECT tag.*
         FROM post_to_tag JOIN tag ON post_to_tag.tag = tag.id
         WHERE post_to_tag.post = ? |] . Only

getAuthors :: Connection -> Id Post -> IO [Author]
getAuthors c =
  query c
    [sql|SELECT author.*
         FROM post_to_author JOIN author ON
           post_to_author.author = author.id
         WHERE post_to_author.post=? |] . Only

getPostsByTags :: Connection -> [Id Tag] -> IO [Post]
getPostsByTags conn = query conn "SELECT * FROM post WHERE tag IN ?" . Only . In

getPostsByDateRange :: Connection -> (UTCTime, UTCTime) -> IO [Post]
getPostsByDateRange conn = query conn
  "SELECT * FROM post WHERE post.created >= ? AND post.created < ?"

getPostsNewer, getPostsOlder :: Connection -> UTCTime -> IO [Post]
getPostsNewer c = query c "SELECT * FROM post WHERE post.created >= ?" . Only
getPostsOlder c = query c "SELECT * FROM post WHERE post.created < ?"  . Only

--------------------------------------------------------------------------------
