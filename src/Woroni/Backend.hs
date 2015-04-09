{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
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
import           Data.Fixed
import qualified Language.Haskell.TH as TH
--------------------------------------------------------------------------------
import qualified Data.Vector as V
--------------------------------------------------------------------------------
import System.Clock
--------------------------------------------------------------------------------
import Hasql
import Hasql.Backend  hiding (Tx)
import Hasql.Postgres
--------------------------------------------------------------------------------
import Woroni.Prelude

deriving instance Show Unknown

newtype Id a = Id Int
  deriving (Eq,Ord,Show,CxValue Postgres)

class Schema a where
  lookupId :: Id a -> Tx Postgres s (Maybe a)
  insert  :: a -> Tx Postgres s (Id a)
  schema  :: proxy a -> Tx Postgres s ()

data LoadT :: (* -> *) -> Bool -> * -> * where
  Load  :: !(m a) -> LoadT m false a
  Got   :: !a     -> LoadT m 'True a

--------------------------------------------------------------------------------
-- Data types
--------------------------------------------------------------------------------

data Post = Post
  { postId      :: {-# UNPACK #-} !(Id Post)
  , postFeature :: !Bool
  , postTitle   :: !Text
  , postContent :: !Text
  , postCreated :: !UTCTime
  , postUpdated :: !(Maybe UTCTime)
  } deriving (Show,Eq,Ord)

data Thread = Thread
  { post           :: !Post
  , postAuthors    :: !(Vector Author)
  , postTags       :: !(Vector Tag)
  , threadComments :: !(Vector Comment)
  }

data Comment = Comment
  { commentId      :: {-# UNPACK #-} !(Id Comment)
  , commentParent  :: {-# UNPACK #-} !(Id Post)
  , commentContent :: !Text
  , commentCreated :: !UTCTime
  , commentAuthor  :: !(Id Author)
  , commentAddress :: !ByteString
  } deriving (Show,Eq,Ord)

data Tag = Tag
  { tagId   :: {-# UNPACK #-} !(Id Tag)
  , tagName :: !Text
  } deriving (Show,Eq,Ord)

data Author = Author
  { authorId      :: {-# UNPACK #-} !(Id Author)
  , authorAddress :: !ByteString
  , authorName    :: !Text
  , authorEmail   :: !(Maybe Text)
  } deriving (Show,Eq,Ord)

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
    if authorId > Id 0
    then authorId <$ unitEx updateAuthor
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
    WHERE post_to_tag.post = ?
        |] pid)

getPostAuthors :: Id Post -> Tx Postgres s (Vector Author)
getPostAuthors pid = fmap (^.re _Author) <$> vectorEx
  ([stmt|
    SELECT author.* FROM author
    JOIN post_to_author ON author.id = post_to_author.author
    WHERE post_to_author.post = ?
        |] pid)

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
  :: [Id Author] -- ^ Authors
  -> [Id Tag]    -- ^ Tags
  -> Bool        -- ^ Whether the post is a "feature" or not
  -> Text        -- ^ Title
  -> Text        -- ^ HTML content
  -> Session Postgres IO ()
newPost authors tags feature title content = do
  now <- liftIO getCurrentTime

  tx (Just (ReadUncommitted, Just True)) $ do
    postId <- insert Post
      { postId      = defaultId
      , postTitle   = title
      , postContent = content
      , postFeature = feature
      , postCreated = now
      , postUpdated = Nothing
      }

    forM_ authors
      (unitEx . [stmt|INSERT INTO post_to_author VALUES(?,?)|] postId)

    forM_ tags
      (unitEx . [stmt|INSERT INTO post_to_tag VALUES(?,?)|] postId)

instance Schema Post where
  schema _ = do
    unitEx [stmt|
      CREATE TABLE post
      ( id      serial  PRIMARY KEY
      , feature boolean NOT NULL
      , summary text    NOT NULL
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

  lookupId pid = do
    c <- maybeEx ([stmt|SELECT * FROM post WHERE post.id = ? LIMIT 1|] pid)
    case c of
      Just
        (postId,postFeature,_postSummary :: Text
        ,postTitle,postContent,postCreated
        ,postUpdated) -> return (Just Post{..})
      Nothing -> return Nothing

  insert p@Post{..} | postId > Id 0 = do
    fmap (const postId) . unitEx $
      [stmt|UPDATE post SET
              feature = ?, summary = ?, title = ?, content = ?, updated = now()
            WHERE post.id = ? |]
      postFeature
      ("summary lol" :: Text)
      postTitle
      postContent
      postId

  insert p@Post{..} | otherwise = do
    pid <- fmap one . singleEx $
      [stmt|INSERT INTO post VALUES(default,?,?,?,?,now(),null) RETURNING id|]
      postFeature
      ("summary lol" :: Text)
      postTitle
      postContent
    let p' = p{postId = pid}
    return pid

--------------------------------------------------------------------------------
instance Schema Comment where
  schema _ = unitEx [stmt|
    CREATE TABLE comment
    ( id        int         PRIMARY KEY
    , parent    int         NOT NULL REFERENCES post(id)
    , content   text        NOT NULL
    , created   timestamptz NOT NULL
    , author    int         NOT NULL REFERENCES author(id)
    , address   bytea       NOT NULL
    ) |]

  insert Comment{..} =
    if commentId > Id 0
    then fmap (const commentId) . unitEx $
      [stmt|UPDATE comment
            SET parent = ?, content = ?, created = ?, author = ?, address = ?
            WHERE authorId = ?|]
      commentParent commentContent commentCreated commentAuthor commentAddress
      commentId
    else fmap one . singleEx $
      [stmt|INSERT INTO author VALUES(default,?,?,?,?,?) RETURNING id|]
      commentParent commentContent commentCreated commentAuthor commentAddress

  lookupId commentId = preview (_Just.re _Comment) <$> maybeEx query
   where
    query = [stmt|SELECT * FROM comment WHERE comment.id = ?|] commentId

--------------------------------------------------------------------------------
-- Thread
--------------------------------------------------------------------------------

measure :: IO a -> IO (Fixed E12, a)
measure f = do
  t0 <- getTime ThreadCPUTime
  x <- f
  t1 <- getTime ThreadCPUTime
  return (1.0e-9 * fromIntegral (nsec t1 - nsec t0), x)

getThread :: Id Post -> Tx Postgres s (Post,
                                       Vector Tag,
                                       Vector (Id Author, Text))
getThread pid = getPostAuthorsTags <$> singleEx threadQuery
 where
  getPostAuthorsTags
   ( postId
   , postFeature
   , _postSummary :: Text
   , postTitle
   , postContent
   , postCreated
   , postUpdated
   , tagIds
   , tagNames
   , authorIds
   , authorNames) = (Post{..},
                     V.zipWith (Tag . Id) tagIds tagNames,
                     V.zipWith ((,) . Id) authorIds authorNames
                    )

  threadQuery :: Stmt Postgres
  threadQuery =
    [stmt|
     SELECT
       post.*,
       array_agg(tag.id),
       array_agg(tag.name),
       array_agg(author.id),
       array_agg(author.name)
     FROM post
     JOIN post_to_tag ON post_to_tag.post = post.id
     JOIN tag ON tag.id = post_to_tag.tag
     JOIN post_to_author ON post_to_author.post = post.id
     JOIN author ON author.id = post_to_author.author
     WHERE post.id = ?
     GROUP BY post.id |]
    pid

getThreadD :: Id Post -> Tx Postgres s (Post, Vector Tag, Vector Author)
getThreadD pid = do
  post    <- lookupId pid
  tags    <- getPostTags pid
  authors <- getPostAuthors pid
  return (fromJust post, tags, authors)

--------------------------------------------------------------------------------
-- Utility

defaultId :: Id a
defaultId = Id (-1)

type family Result f where
  Result (a -> b) = Result b
  Result r = r

-- | Just a stupid syntactic thing
create :: forall f s. Schema (Result f) => f -> Tx Postgres s ()
create _ = schema (Nothing :: Maybe (Result f))
