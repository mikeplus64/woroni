{-# LANGUAGE DataKinds, DeriveDataTypeable, GADTs, GeneralizedNewtypeDeriving,
             NamedFieldPuns, OverloadedStrings, PolyKinds, QuasiQuotes,
             RecordWildCards, TemplateHaskell, TypeFamilies #-}
module Schema where
import Control.Applicative
import Control.Lens
import Control.Monad

import Data.Maybe (listToMaybe)

import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import Data.String
import Data.Time
import Data.Typeable
import Data.Vector   (Vector, fromList)
import Data.Word     (Word)

import Data.Aeson
import Data.Aeson.TH
import Data.ByteString (ByteString)

import Database.PostgreSQL.Query.TH               (deriveFromRow, deriveToRow)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.TypeInfo.Static

initDB :: IO Connection
initDB = connect defaultConnectInfo{connectUser = "mike",connectDatabase="postgres"}

type Name  = Text
type Email = Text

newtype Id a = Id Int deriving (Eq, Show, Ord, ToJSON, FromJSON)
fromId :: Id a -> Int
fromId (Id a) = a

instance FromField (Id a) where fromField f mbs = fmap Id (fromField f mbs)
instance ToField   (Id a) where toField (Id a) = toField a

data Post = Post
  { postId      :: !(Id Post)
  , postTitle   :: !Text
  , postContent :: !Text
  , postCreated :: !UTCTime
  , postUpdated :: !(Maybe UTCTime)
  } deriving (Eq, Ord, Show)

data Author = Author
  { authorId    :: !(Id Author)
  , authorAddr  :: !Inet
  , authorName  :: !Name
  , authorEmail :: !(Maybe Email)
  } deriving (Eq, Ord, Show)

data Comment = Comment
  { commentId      :: !(Id Comment)
  , commentParent  :: !(Id Post)
  , commentAuthor  :: !(Id Author)
  , commentContent :: !Text
  , commentCreated :: !UTCTime
  , commentUpdated :: !(Maybe UTCTime)
  } deriving (Eq, Ord, Show)

data Tag = Tag { tagId :: !(Id Tag), tagName :: !Text }
  deriving (Eq, Ord, Show)

data Wrote  = Wrote  !(Id Post) !(Id Author) deriving (Eq, Ord, Show)
data Tagged = Tagged !(Id Post) !(Id Tag)    deriving (Eq, Ord, Show)

newtype Inet = Inet ByteString
  deriving (Show, Eq, Ord, Typeable)

instance FromJSON Inet where
  parseJSON i = Inet . T.encodeUtf8 <$> parseJSON i

instance ToJSON Inet where
  toJSON (Inet i) = toJSON (T.decodeUtf8 i)

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

deriveJSON defaultOptions ''Post    ; deriveFromRow ''Post    ; deriveToRow ''Post
deriveJSON defaultOptions ''Author  ; deriveFromRow ''Author  ; deriveToRow ''Author
deriveJSON defaultOptions ''Comment ; deriveFromRow ''Comment ; deriveToRow ''Comment
deriveJSON defaultOptions ''Tag     ; deriveFromRow ''Tag     ; deriveToRow ''Tag
deriveJSON defaultOptions ''Tagged  ; deriveFromRow ''Tagged  ; deriveToRow ''Tagged
deriveJSON defaultOptions ''Wrote   ; deriveFromRow ''Wrote   ; deriveToRow ''Wrote


class HasId a where
  getById :: Connection -> Id a -> IO (Maybe a)

instance HasId Author where
  getById conn id' = listToMaybe `fmap` query conn
    [sql|SELECT * FROM author WHERE author.id=? LIMIT 1;|]
    (Only id')

instance HasId Post where
  getById conn id' = listToMaybe `fmap` query conn
    [sql|SELECT * FROM post WHERE post.id=? LIMIT 1;|]
    (Only id')

instance HasId Comment where
  getById conn id' = listToMaybe `fmap` query conn
    [sql|SELECT * FROM comment WHERE comment.id=? LIMIT 1;|]
    (Only id')

instance HasId Tag where
  getById conn id' = listToMaybe `fmap` query conn
    [sql|SELECT * FROM tag WHERE tag.id=? LIMIT 1;|]
    (Only id')

--------------------------------------------------------------------------------
-- Post queries

getComments :: Query
getComments = [sql|SELECT comment.* FROM comment WHERE comment.parent=?
                   ORDER BY comment.created |]


getCommentAuthor :: Connection -> Id Comment -> IO (Maybe Author)
getCommentAuthor c cid = listToMaybe <$> query c
  [sql|SELECT author.*
       FROM comment JOIN author ON comment.author = author.id
       WHERE comment.id=? |] (Only cid)

getTags :: Connection -> Id Post -> IO [Tag]
getTags c = query c
  [sql|SELECT tag.*
       FROM post_to_tag JOIN tag ON post_to_tag.tag = tag.id
       WHERE post_to_tag.post = ? |] . Only

getAuthors :: Connection -> Id Post -> IO [Author]
getAuthors c = query c
  [sql|SELECT author.*
       FROM post_to_author JOIN author ON
         post_to_author.author = author.id
       WHERE post_to_author.post=? |] . Only

getPostsByTags :: Connection -> [Id Tag] -> IO [Post]
getPostsByTags c = query c [sql|SELECT * FROM post WHERE post.tag IN ?|] . Only . In

getPostsByDateRange :: Connection -> (UTCTime, UTCTime) -> IO [Post]
getPostsByDateRange c = query c
  [sql|SELECT * FROM post WHERE post.created >= ? AND post.created < ?|]

getPostsNewer, getPostsOlder :: Connection -> UTCTime -> IO [Post]
getPostsNewer c = query c [sql|SELECT * FROM post WHERE post.created >= ?|].Only
getPostsOlder c = query c [sql|SELECT * FROM post WHERE post.created < ?|].Only

--------------------------------------------------------------------------------
-- Misc

getAllTags :: Connection -> IO [Tag]
getAllTags c = query_ c [sql|SELECT * FROM tag|]

getAllAuthors :: Connection -> IO [Tag]
getAllAuthors c = query_ c [sql|SELECT * FROM author, post_to_author
                                WHERE post_to_author.author = author.id|]

--------------------------------------------------------------------------------
-- Comments

data CommentAuthor
  = Known !(Id Author)
  | Named !Inet !Text
  | Anonymous !Inet

addAuthor :: Connection -> Name -> Inet -> Maybe Email -> IO (Maybe (Id Author))
addAuthor conn name addr em =
  fmap fromOnly . listToMaybe <$> query conn
    [sql|INSERT INTO author VALUES(default,?,?,?) RETURNING author.id|]
    (addr, name, em)

addComment
  :: Connection
  -> CommentAuthor
  -> Id Post -- ^ the post
  -> Text    -- ^ content
  -> IO Bool
addComment conn cauthor parent content =
  case cauthor of
    Known aid ->
      (1 ==) <$> execute conn
        [sql|INSERT INTO comment VALUES(default,?,?,?,default,null)|]
        (parent, aid, content)

    Named addr name -> do
      aid' <- addAuthor conn name addr Nothing
      case aid' of
        Just aid -> addComment conn (Known aid) parent content
        Nothing  -> return False


    Anonymous addr -> do
      aid' <- addAuthor conn "Anonymous" addr Nothing
      case aid' of
        Just aid -> addComment conn (Known aid) parent content
        Nothing  -> return False

--------------------------------------------------------------------------------
-- Post authoring

addPost :: Connection -> [Id Tag] -> [Id Author] -> Text -> Text -> IO (Id Post)
addPost con tagIds authorIds title content = do
  [Only pid] <-
    query con
    [sql|INSERT INTO post
         VALUES (default,?,?,default,null)
         RETURNING post.id|]
    (title,content)

  tagOks <- forM tagIds $ \tid ->
    execute con
    [sql|INSERT INTO post_to_tag VALUES(?, ?)|]
    (pid, tid)

  authOks <- forM authorIds $ \aid ->
    execute con
    [sql|INSERT INTO post_to_author VALUES(?, ?)|]
    (pid, aid)

  return pid


