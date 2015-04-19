{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NullaryTypeClasses         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Woroni.Backend where
--------------------------------------------------------------------------------
import qualified Data.Aeson          as JSON
import qualified Data.Aeson.TH       as JSON
import           Data.Char           (toLower)
import qualified Language.Haskell.TH as TH
--------------------------------------------------------------------------------
import Hasql
import Hasql.Backend  hiding (Tx)
import Hasql.Postgres
--------------------------------------------------------------------------------
import Woroni.Prelude
--------------------------------------------------------------------------------

newtype Id a = Id Int32 deriving (Eq,Ord,Show,Generic,CxValue Postgres)
fromId :: Id a -> Id'
fromId (Id a) = a

type Id' = Int32

class Schema a where
  lookupId :: Id a -> Tx Postgres s (Maybe a)
  insert  :: a -> Tx Postgres s (Id a)
  schema  :: proxy a -> Tx Postgres s ()

data Paginate a = Paginate
  { pbegin :: {-# UNPACK #-} !(Id a)
  , plimit :: {-# UNPACK #-} !Int32
  , page   :: !(Rows a)
  } deriving (Show,Eq,Ord)

data Pagination a = Page
  { begin :: {-# UNPACK #-} !(Id a)
  , limit :: !Int32
  }

--------------------------------------------------------------------------------
-- Data types
--------------------------------------------------------------------------------

data Post = Post
  { postId      :: {-# UNPACK #-} !Id'
  , postFeature :: !Bool
  , postTitle   :: !Text
  , postContent :: !Text
  , postCreated :: !UTCTime
  , postUpdated :: !(Maybe UTCTime)
  } deriving (Show,Eq,Ord,Generic)

data Article = Article
  { post         :: !(Row Post)
  , postAuthors  :: !(Rows Author)
  , postTags     :: !(Rows Tag)
  , postComments :: !(Paginate Comment)
  } deriving (Show,Eq,Ord)

data Comment = Comment
  { commentId       :: {-# UNPACK #-} !Id'
  , commentThreadId :: {-# UNPACK #-} !Id'
  , commentParent   :: {-# UNPACK #-} !Id'
  , commentContent  :: !Text
  , commentCreated  :: !UTCTime
  , commentAuthor   :: !Id'
  , commentAddress  :: !ByteString
  } deriving (Show,Eq,Ord,Generic)

data Tag = Tag
  { tagId   :: {-# UNPACK #-} !Id'
  , tagName :: !Text
  } deriving (Show,Eq,Ord,Generic)

data Author = Author
  { authorId      :: {-# UNPACK #-} !Id'
  , authorAddress :: !ByteString
  , authorName    :: !Text
  , authorEmail   :: !(Maybe Text)
  } deriving (Show,Eq,Ord,Generic)

instance CxRow Postgres Post
instance ViaFields Post
instance ViaFields Comment
instance ViaFields Author
instance ViaFields Tag

instance JSON.FromJSON ByteString where
  parseJSON v = fmap encodeUtf8 (JSON.parseJSON v)

instance JSON.ToJSON ByteString where
  toJSON v = JSON.toJSON (decodeUtf8 v)

concat <$> forM
  [ ''Id, ''Post, ''Author, ''Comment, ''Tag ]
  (\name -> do
       let options = JSON.defaultOptions
             { JSON.fieldLabelModifier =
                   fmap toLower . drop (length (TH.nameBase name))
             }
       json   <- JSON.deriveJSON options name
       prisms <- makePrisms name
       return (json ++ prisms))

--------------------------------------------------------------------------------
-- SQL Schema
--------------------------------------------------------------------------------

getPool :: IO (Pool Postgres)
getPool = acquirePool
  (StringSettings "user=mike host=localhost dbname=postgres")
  (fromJust (poolSettings 8 10))

setSchema :: Tx Postgres s ()
setSchema = do
  create Tag
  create Author
  create Post
  create Comment

--------------------------------------------------------------------------------
-- Schema instances

txMode :: TxMode
txMode = Just (RepeatableReads, Just True)

one :: Identity a -> a
one = coerce

rows :: a -> m (Rows a) -> m (Rows a)
rows _ a = a

--------------------------------------------------------------------------------
-- Tag
--------------------------------------------------------------------------------

instance Schema Tag where
  lookupId tid = preview (_Just.re _Tag) <$>
    maybeEx ([stmt|SELECT * FROM tag WHERE tag.id = ? LIMIT 1|] tid)

  insert (Tag _ n) = fmap one . singleEx $
    [stmt|INSERT INTO tag VALUES(default,?) RETURNING id|] n

  schema _ = unitEx [stmt|
    CREATE TABLE tag
      ( id     serial PRIMARY KEY
      , name   text   NOT NULL
      )|]

--------------------------------------------------------------------------------
-- Author
--------------------------------------------------------------------------------

instance Schema Author where
  schema _ = do
    unitEx [stmt|
      CREATE TABLE author
      ( id      serial   PRIMARY KEY
      , address bytea
      , name    text
      , email   text
      )|]

  lookupId aid = preview (_Just.re _Author) <$> maybeEx query
   where query = [stmt|SELECT * FROM author WHERE author.id = ? LIMIT 1|] aid

  insert Author{..} =
    if authorId > 0
    then Id authorId <$ unitEx updateAuthor
    else one <$> singleEx insertAuthor
   where
    insertAuthor :: Stmt Postgres
    insertAuthor =
      [stmt|INSERT INTO author VALUES(default,?,?,?) RETURNING id|]
      authorAddress authorName authorEmail

    updateAuthor :: Stmt Postgres
    updateAuthor =
      [stmt|UPDATE author SET address = ?, name = ?, email = ?
            WHERE author.id = ? |]
      authorAddress authorName authorEmail authorId

--------------------------------------------------------------------------------
getPostTags :: Id Post -> Tx Postgres s (Vector Tag)
getPostTags pid = fmap (^.re _Tag) <$> vectorEx
  ([stmt|
    SELECT tag.* FROM tag
    JOIN post_to_tag ON tag.id = post_to_tag.tag
    WHERE post_to_tag.post = ? |] pid)

getPostAuthors :: Id Post -> Tx Postgres s (Vector Author)
getPostAuthors pid = fmap (^.re _Author) <$> vectorEx
  ([stmt|
    SELECT author.* FROM author
    JOIN post_to_author ON author.id = post_to_author.author
    WHERE post_to_author.post = ? |] pid)

  {-
loadPostAuthors :: Post -> Session Postgres IO (Vector Author)
loadPostAuthors Post{postAuthors = authors} = case authors of
  Got   t -> return t
  Load f -> tx Nothing f

insertPostAuthors :: Post -> Tx Postgres s ()
insertPostAuthors Post{postId,postAuthors} = case postAuthors of
  Got as -> forM_ as $ \author@Author{authorId} -> do
    aid <- if authorId > Id 0 then return authorId else insert author
    unitEx $ [stmt|INSERT INTO post_to_author VALUES(?,?)|] postId aid
  _      -> return ()

insertPostTags :: Post -> Tx Postgres s ()
insertPostTags Post{postId,postTags} = case postTags of
  Got as -> forM_ as $ \tag@(Tag tagId _)-> do
    tid <- if tagId > Id 0 then return tagId else insert tag
    unitEx $ [stmt|INSERT INTO post_to_tag VALUES(?,?)|] postId tid
  _      -> return ()
-}

newPost
  :: Pool Postgres
  -> [Id Author] -- ^ Authors
  -> [Id Tag]    -- ^ Tags
  -> Bool        -- ^ Whether the post is a "feature" or not
  -> Text        -- ^ Title
  -> Text        -- ^ HTML content
  -> IO (Id Post)
newPost pool authors tags feature title content = do
  now <- getCurrentTime
  pid <- session pool $ tx (Just (ReadUncommitted, Just True)) $ do
    pid <- insert Post
      { postId      = fromId defaultId
      , postTitle   = title
      , postContent = content
      , postFeature = feature
      , postCreated = now
      , postUpdated = Nothing
      }
    mapM_ (unitEx . [stmt|INSERT INTO post_to_author VALUES(?,?)|] pid) authors
    mapM_ (unitEx . [stmt|INSERT INTO post_to_tag    VALUES(?,?)|] pid) tags
    return pid
  print pid
  return (either undefined id pid)

instance Schema Post where
  schema _ = do
    unitEx [stmt|
      CREATE TABLE post
      ( id      serial  PRIMARY KEY
      , feature boolean NOT NULL
      , title   text    NOT NULL
      , content text    NOT NULL
      , created timestamptz NOT NULL DEFAULT now()
      , updated timestamptz DEFAULT null
      )|]
    unitEx [stmt|
      CREATE TABLE post_to_author
      ( post   int NOT NULL REFERENCES post(id)
      , author int NOT NULL REFERENCES author(id)
      , UNIQUE(post, author)
      )|]
    unitEx [stmt|
      CREATE TABLE post_to_tag
      ( post   int NOT NULL REFERENCES post(id)
      , tag    int NOT NULL REFERENCES tag(id)
      , UNIQUE(post, tag)
      )|]

  lookupId = maybeEx . [stmt|SELECT * FROM post WHERE post.id = ? LIMIT 1|]

  insert Post{..} =
    if postId > 0
    then fmap (const (Id postId)) . unitEx $
      [stmt|UPDATE post SET feature = ?, title = ?, content = ?, updated = now()
            WHERE post.id = ? |]
      postFeature
      postTitle
      postContent
      postId
    else fmap one . singleEx $
      [stmt|INSERT INTO post VALUES(default,?,?,?,?,null) RETURNING id|]
      postFeature
      postTitle
      postContent
      postCreated

--------------------------------------------------------------------------------
instance Schema Comment where
  schema _ = unitEx [stmt|
    CREATE TABLE comment
    ( id        serial      PRIMARY KEY
    , local_id  int         NOT NULL
    , parent    int         NOT NULL REFERENCES post(id)
    , content   text        NOT NULL
    , created   timestamptz NOT NULL
    , author    int         NOT NULL REFERENCES author(id)
    , address   bytea       NOT NULL
    , UNIQUE(parent, local_id)
    ) |]

  insert Comment{..} =
    if commentId > 0
    then fmap (const (Id commentId)) . unitEx $
      [stmt|UPDATE comment SET parent = ?, content = ?, author = ?, address = ?
            WHERE authorId = ?|]
      commentParent commentContent commentAuthor commentAddress commentId
    else
      fmap one . singleEx $
        [stmt|INSERT INTO comment VALUES
            (default,
             (SELECT coalesce(max(local_id)+1, 0) FROM comment WHERE comment.parent = ?),
             ?, ?, ?, ?, ?)
            RETURNING id|]
        commentParent
        commentParent
        commentContent
        commentCreated
        commentAuthor
        commentAddress


  lookupId commentId = preview (_Just.re _Comment) <$> maybeEx query
   where
    query = [stmt|SELECT * FROM comment WHERE comment.id = ?|] commentId

--------------------------------------------------------------------------------
-- Article
--------------------------------------------------------------------------------

getArticle :: Id Post -> Pagination Comment -> Tx Postgres s Article
getArticle pid pagination = do
  (post,tags,authors,comments) <- singleEx
    ([stmt|
      SELECT
        row(post.*)          "post",
        array_agg(tag.*)     "tags",
        array_agg(author.*)  "authors",
        array_agg(comment.*) "comments"
      FROM post
        LEFT JOIN post_to_tag ON post_to_tag.post = post.id
        LEFT JOIN tag         ON tag.id = post_to_tag.tag
        LEFT JOIN post_to_author ON post_to_author.post = post.id
        LEFT JOIN author         ON author.id = post_to_author.author
        LEFT JOIN comment ON
          comment.parent = post.id AND
          comment.local_id >= ? AND comment.local_id < ?
      WHERE post.id = ?
      GROUP BY post.id
          |]
    (begin pagination)
    (fromId (begin pagination) + limit pagination)
    pid)
  return $! Article post tags authors $!
    Paginate (Id 0) 10 comments

--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------

defaultId :: Id a
defaultId = Id (-1)

type family Result f where
  Result (a -> b) = Result b
  Result r = r

-- | Just a stupid syntactic thing
create :: forall f s. Schema (Result f) => f -> Tx Postgres s ()
create _ = schema (Nothing :: Maybe (Result f))
