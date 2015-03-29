{-# LANGUAGE DataKinds, DeriveDataTypeable, GADTs, GeneralizedNewtypeDeriving,
             NamedFieldPuns, OverloadedStrings, PolyKinds, QuasiQuotes,
             RecordWildCards, TemplateHaskell, TypeFamilies #-}
module DB.Schema where
import Control.Applicative

import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString    (ByteString)
import           Data.Maybe
import           Data.Text          (Text)
import qualified Data.Text.Encoding as T
import           Data.Time
import           Data.Typeable

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
