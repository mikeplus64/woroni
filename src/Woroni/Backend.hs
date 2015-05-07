{-# LANGUAGE ConstraintKinds, DataKinds, DeriveGeneric, FlexibleContexts,
             FlexibleInstances, GADTs, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, MultiWayIf, NamedFieldPuns,
             NoImplicitPrelude, NullaryTypeClasses, OverloadedStrings,
             PolyKinds, QuasiQuotes, RankNTypes, RecordWildCards,
             ScopedTypeVariables, StandaloneDeriving, TemplateHaskell,
             TypeFamilies #-}
module Woroni.Backend
  ( Schema(..)
  , Id(..), Id', fromId
    -- * Main types
  , Post(..)
  , Author(..)
  , Tag(..)
  , Comment(..)
  , AComment(..)
    -- * Posts queries
  , Article(..)
  , Summary(..)
  , getArticle
    -- * Editing
  , newPost
  , updatePost
  , removePost
    -- * Comments
  , newComment
  , removeComment
    -- * Paginated things
  , Pages(..)
  , Page(..)
  , MaybeId(..)
    -- * Search
  , postsSearch
  , tagSearch
    -- * Utility
  , getPool
  , setSchema
  , txMode
  , trydb
  , Row(..), Rows(..)
  , fromRows
  , fromRow
  ) where
--------------------------------------------------------------------------------
import Hasql
import Hasql.Backend  hiding (Tx)
import Hasql.Postgres
--------------------------------------------------------------------------------
import Woroni.Backend.Markdown
import Woroni.Backend.Summarise (summarise)
import Woroni.Prelude
--------------------------------------------------------------------------------

newtype Id a = Id Int32 deriving (Eq,Ord,Show,Generic,CxValue Postgres)
fromId :: Id a -> Id'
fromId (Id a) = a

type Id' = Int32

class Schema a where
  lookupId :: Id a -> Tx Postgres s (Maybe a)
  add   :: a -> Tx Postgres s (Id a)
  schema   :: proxy a -> Tx Postgres s ()

data Page :: Maybe * -> * -> * where
  Page
    :: {-# UNPACK #-} !Int32 -- ^ offset
    -> !(Vector a)
    -> Page Nothing a
  PageSrc
    :: {-# UNPACK #-} !(Id p) -- ^ source
    -> {-# UNPACK #-} !Int32  -- ^ offset
    -> !(Vector a)
    -> Page (Just p) a

instance Eq a => Eq (Page p a) where
  Page i0 a0 == Page i1 a1 = i0 == i1 && a0 == a1
  PageSrc s0 i0 a0 == PageSrc s1 i1 a1 = s0 == s1 && i0 == i1 && a0 == a1
  _ == _ = False

instance Show a => Show (Page p a) where
  showsPrec i (Page i0 a0) = showsPrec i (i0,a0)
  showsPrec i (PageSrc s0 i0 a0) = showsPrec i (s0,i0,a0)

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
  } deriving (Show,Eq,Generic)

data Article = Article
  { post         :: !Post
  , postAuthors  :: !(Vector Author)
  , postTags     :: !(Vector Tag)
  , postComments :: !(Page (Just Post) AComment)
  } deriving (Show,Eq)

data Summary = Summary
  { summaryPost    :: !(Row Post)
  , summaryAuthors :: !(Rows Author)
  , summaryTags    :: !(Rows Tag)
  } deriving (Show,Eq,Generic)

data Comment = Comment
  { commentId       :: {-# UNPACK #-} !Id'
  , commentLocalId  :: {-# UNPACK #-} !Id'
  , commentParent   :: {-# UNPACK #-} !Id'
  , commentContent  :: !Text
  , commentCreated  :: !UTCTime
  , commentAuthorId :: !Id'
  , commentAddress  :: !ByteString
  } deriving (Show,Eq,Generic)

data AComment = AComment
  { acommentId         :: {-# UNPACK #-} !Id'
  , acommentLocalId    :: {-# UNPACK #-} !Id'
  , acommentParent     :: {-# UNPACK #-} !Id'
  , acommentContent    :: !Text
  , acommentCreated    :: !UTCTime
  , acommentAuthorId   :: !Id'
  , acommentAddress    :: !ByteString
  , cauthorId          :: {-# UNPACK #-} !Id'
  , cauthorName        :: !Text
  , cauthorEmail       :: !(Maybe Text)
  , cauthorDescription :: !(Maybe Text)
  } deriving (Show,Eq,Generic)

data Tag = Tag
  { tagId   :: {-# UNPACK #-} !Id'
  , tagName :: !Text
  } deriving (Show,Eq,Generic)

data Author = Author
  { authorId          :: {-# UNPACK #-} !Id'
  , authorName        :: !Text
  , authorEmail       :: !(Maybe Text)
  , authorDescription :: !(Maybe Text)
  } deriving (Show,Eq,Generic)

instance CxRow Postgres Post
instance CxRow Postgres Comment
instance CxRow Postgres Summary
instance CxRow Postgres Author
instance CxRow Postgres Tag
instance ViaFields Post
instance ViaFields Comment
instance ViaFields AComment
instance ViaFields Author
instance ViaFields Tag

--------------------------------------------------------------------------------
-- SQL Schema
--------------------------------------------------------------------------------

getPool :: IO (Pool Postgres)
getPool = acquirePool
  (StringSettings "user=postgres host=localhost dbname=woroni")
  (fromJust (poolSettings 8 10))

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
  schema _ = unitEx [stmt|
    CREATE TABLE IF NOT EXISTS tag
      ( id     serial PRIMARY KEY
      , name   text   NOT NULL
      , UNIQUE(id, name)
      , UNIQUE(name)
      )|]

  lookupId = maybeEx . [stmt|SELECT * FROM tag WHERE tag.id = ? LIMIT 1|]

  add (Tag _ n) = fmap one . singleEx $
    [stmt|INSERT INTO tag VALUES(default,?) RETURNING id|] n

--------------------------------------------------------------------------------
-- Author
--------------------------------------------------------------------------------

instance Schema Author where
  schema _ = do
    unitEx [stmt|
      CREATE TABLE IF NOT EXISTS author
      ( id      serial   PRIMARY KEY
      , name    text     NOT NULL
      , email   text
      , description text
      )|]
    unitEx [stmt|INSERT INTO author (id, name, email)
                 SELECT 0, 'Anonymous', NULL
                 WHERE NOT EXISTS (SELECT 1 FROM author WHERE id = 0)|]

  lookupId = maybeEx . [stmt|SELECT * FROM author WHERE author.id = ? LIMIT 1|]

  add Author{..} =
    if authorId > 0
    then Id authorId <$ unitEx updateAuthor
    else one <$> singleEx addAuthor
   where
    addAuthor :: Stmt Postgres
    addAuthor =
      [stmt|INSERT INTO author VALUES(default,?,?,?) RETURNING id|]
      authorName
      authorEmail
      (renderMarkdownTrusted <$> authorDescription)

    updateAuthor :: Stmt Postgres
    updateAuthor =
      [stmt|UPDATE author SET name = ?, email = ?, description = ?
            WHERE author.id = ? |]
      authorName
      authorEmail
      (renderMarkdownTrusted <$> authorDescription)
      authorId

--------------------------------------------------------------------------------
removePost :: Id Post -> Maybe Text -> UTCTime -> Tx Postgres s Bool
removePost pid reason now = do
  mpost <- maybeEx ([stmt|DELETE FROM post WHERE id = ? RETURNING *|] pid)
  case mpost of
    Just Post{..} -> do
      unitEx $ [stmt|INSERT INTO post_deleted VALUES(?, ?, ?, ?, ?) |]
        pid postTitle postCreated now reason
      return True
    Nothing -> return False

updatePost
  :: Id Post -- ^ Post to update
  -> Text    -- ^ New title
  -> Text    -- ^ New Markdown content
  -> UTCTime -- ^ Update date
  -> Tx Postgres s ()
updatePost pid title content now = do
  let md = renderMarkdownTrusted content
  unitEx $ [stmt|UPDATE post SET title = ?, content = ?, updated = ?
                 WHERE id = ?|]
    title
    md
    now
    pid
  unitEx $ [stmt|UPDATE summary SET content = ? WHERE post = ?|]
    (summarise id md)
    pid
  unitEx ([stmt|INSERT INTO post_updates VALUES(?,?)|] pid now)

newPost
  :: [Id Author] -- ^ Authors
  -> [Id Tag]    -- ^ Tags
  -> Bool        -- ^ Whether the post is a "feature" or not
  -> Text        -- ^ Title
  -> Text        -- ^ Markdown content
  -> UTCTime     -- ^ Add date
  -> Tx Postgres s (Id Post)
newPost authors tags feature title content now = do
  when (null tags) (fail "newPost: post must have tags")
  pid <- add Post
    { postId      = fromId defaultId
    , postTitle   = title
    , postContent = renderMarkdownTrusted content
    , postFeature = feature
    , postCreated = now
    , postUpdated = Nothing
    }
  mapM_ (unitEx . [stmt|INSERT INTO post_to_author VALUES(?,?)|] pid)
    (case authors of
      [] -> [Id 0] -- add anonymous
      _  -> authors)
  mapM_ (unitEx . [stmt|INSERT INTO post_to_tag    VALUES(?,?)|] pid) tags
  unitEx ([stmt|INSERT INTO post_updates VALUES(?,?)|] pid now)
  return pid

instance Schema Post where
  schema _ = do
    unitEx [stmt|
      CREATE TABLE IF NOT EXISTS post
      ( id      serial  PRIMARY KEY
      , feature boolean NOT NULL
      , title   text    NOT NULL
      , content text    NOT NULL
      , created timestamptz NOT NULL DEFAULT now()
      , updated timestamptz DEFAULT null
      )|]
    unitEx [stmt|
      CREATE TABLE IF NOT EXISTS summary
      ( post    int  PRIMARY KEY REFERENCES post(id) ON DELETE CASCADE
      , content text NOT NULL
      , UNIQUE(post, content)
      )|]
    unitEx [stmt|
      CREATE TABLE IF NOT EXISTS post_search
      ( post     int      PRIMARY KEY REFERENCES post(id) ON DELETE CASCADE
      , tags     int[]    NOT NULL
      , authors  int[]    NOT NULL
      , document tsvector NOT NULL
      )|]
    unitEx [stmt|
      CREATE TABLE IF NOT EXISTS post_to_author
      ( post   int NOT NULL REFERENCES post(id) ON DELETE CASCADE
      , author int NOT NULL REFERENCES author(id) ON DELETE CASCADE
      , UNIQUE(post, author)
      )|]
    unitEx [stmt|
      CREATE TABLE IF NOT EXISTS post_to_tag
      ( post   int NOT NULL REFERENCES post(id) ON DELETE CASCADE
      , tag    int NOT NULL REFERENCES tag(id) ON DELETE CASCADE
      , UNIQUE(post, tag)
      )|]
    unitEx [stmt|
      CREATE TABLE IF NOT EXISTS post_deleted
      ( post    int         PRIMARY KEY
      , title   text        NOT NULL
      , created timestamptz NOT NULL
      , deleted timestamptz NOT NULL
      , reason  text
      )|]
    unitEx [stmt|
      CREATE TABLE IF NOT EXISTS post_updates
      ( post    int         REFERENCES post(id) ON DELETE CASCADE
      , date    timestamptz NOT NULL
      , UNIQUE(post, date)
      )|]
    unitEx [stmt|
      CREATE OR REPLACE FUNCTION post_document(pid int)
          RETURNS tsvector AS $$
        SELECT to_tsvector('english',
          string_agg(tag.name, ' ') || ' ' ||
          string_agg(author.name, ' ') || ' ' ||
          post.title || ' ' ||
          post.content )
        FROM post
          JOIN post_to_author ON post_to_author.post = pid
          JOIN author         ON author.id = post_to_author.author
          JOIN post_to_tag ON post_to_tag.post = pid
          JOIN tag         ON tag.id = post_to_tag.tag
        WHERE post.id = pid
        GROUP BY post.id
      $$ LANGUAGE 'sql' STABLE |]
    unitEx [stmt|
      CREATE OR REPLACE RULE post_set_document AS
        ON INSERT TO post_updates DO
        INSERT INTO post_search
        SELECT NEW.post,
               (SELECT array_agg(tag.id) FROM tag, post_to_tag WHERE
                 post_to_tag.post = NEW.post AND
                 post_to_tag.tag = tag.id),
               (SELECT array_agg(author.id) FROM author, post_to_author WHERE
                 post_to_author.post = NEW.post AND
                 post_to_author.author = author.id),
               post_document(NEW.post)
        WHERE (NOT EXISTS (SELECT 1 FROM post_search
                           WHERE post_search.post = NEW.post))
        |]
    unitEx [stmt|
      CREATE OR REPLACE RULE post_update_document AS
        ON UPDATE TO post_updates DO
        UPDATE post_search
        SET post = NEW.post,
            tags =
              (SELECT array_agg(tag.id) FROM tag, post_to_tag WHERE
                post_to_tag.post = NEW.post AND
                post_to_tag.tag = tag.id),
            authors =
              (SELECT array_agg(author.id) FROM author, post_to_author WHERE
                post_to_author.post = NEW.post AND
                post_to_author.author = author.id),
            document = post_document(NEW.post)
        WHERE (EXISTS (SELECT 1 FROM post_search
                       WHERE post_search.post = NEW.post))
        |]

  lookupId = maybeEx . [stmt|SELECT * FROM post WHERE post.id = ? LIMIT 1|]

  add Post{..} =
    if postId > 0
    then do
      unitEx $
        [stmt|UPDATE post
              SET feature = ?, title = ?, content = ?, updated = now()
              WHERE post.id = ? |]
        postFeature
        postTitle
        postContent
        postId
      unitEx $
        [stmt|UPDATE summary SET content = ? WHERE post = ?|]
        (summarise id postContent)
        postId
      return (Id postId)
    else do
      pid <- one <$> singleEx
        ([stmt|INSERT INTO post VALUES(default,?,?,?,?,null) RETURNING id|]
         postFeature
         postTitle
         postContent
         postCreated)
      unitEx
        ([stmt|INSERT INTO summary VALUES(?, ?)|]
         pid
         (summarise id postContent))
      return pid

--------------------------------------------------------------------------------

newComment
  :: Id Post
  -> Maybe Text
  -> Maybe Text
  -> Text
  -> ByteString
  -> UTCTime
  -> Tx Postgres s (Id Comment)
newComment pid name email content addr now = do
  aid <- case name of
    Just n -> add Author
      { authorId          = -1
      , authorName        = n
      , authorEmail       = email
      , authorDescription = Nothing
      }
    Nothing -> return (Id 0)
  add $ Comment
    { commentId       = -1
    , commentLocalId  = -1
    , commentParent   = fromId pid
    , commentContent  = renderMarkdown content
    , commentCreated  = now
    , commentAuthorId = fromId aid
    , commentAddress  = addr
    }

removeComment
  :: Id Comment
  -> Maybe Text
  -> UTCTime
  -> Tx Postgres s Bool
removeComment cid reason now = do
  mcomment <- maybeEx ([stmt|DELETE FROM comment WHERE id = ? RETURNING *|] cid)
  case mcomment of
    Just Comment{..} -> do
      unitEx $ [stmt|INSERT INTO deleted_comment VALUES(?, ?, ?, ?, ?, ?) |]
        commentId
        commentLocalId
        commentParent
        commentCreated
        now
        reason
      return True
    Nothing -> return False

instance Schema Comment where
  schema _ = do
    unitEx [stmt|
      CREATE TABLE IF NOT EXISTS comment
      ( id        serial      PRIMARY KEY
      , local_id  int         NOT NULL
      , parent    int         NOT NULL REFERENCES post(id) ON DELETE CASCADE
      , content   text        NOT NULL
      , created   timestamptz NOT NULL
      , author    int         NOT NULL REFERENCES author(id) ON DELETE CASCADE
      , address   bytea       NOT NULL
      , UNIQUE(parent, local_id)
      ) |]
    unitEx [stmt|
      CREATE TABLE IF NOT EXISTS deleted_comment
      ( comment   int         PRIMARY KEY
      , local_id  int         NOT NULL
      , parent    int         NOT NULL REFERENCES post(id) ON DELETE CASCADE
      , created   timestamptz NOT NULL
      , deleted   timestamptz NOT NULL
      , reason    text
      , UNIQUE(parent, local_id)
      ) |]

  add Comment{..} =
    if commentId > 0
    then fmap (const (Id commentId)) . unitEx $
      [stmt|UPDATE comment SET parent = ?, content = ?, author = ?, address = ?
            WHERE authorId = ?|]
      commentParent commentContent commentAuthorId commentAddress commentId
    else
      fmap one . singleEx $
        [stmt|
        INSERT INTO comment VALUES
          (default,
          (SELECT coalesce(max(local_id)+1, 0) FROM comment WHERE parent = ?),
           ?, ?, ?, ?, ?)
          RETURNING id|]
        commentParent
        commentParent
        commentContent
        commentCreated
        commentAuthorId
        commentAddress

  lookupId = maybeEx . [stmt|SELECT * FROM comment WHERE comment.id = ?|]

--------------------------------------------------------------------------------
-- Article
--------------------------------------------------------------------------------

getArticle :: Id Post -> Tx Postgres s (Maybe Article)
getArticle pid = do
  ma <- maybeEx
    ([stmt|
      SELECT
        row(post.*) "post",
        coalesce (
          (SELECT array_agg(author.*) FROM post_to_author
           JOIN author ON post_to_author.author = author.id
           WHERE post_to_author.post = post.id),
          '{}') "authors",
        coalesce (
          (SELECT array_agg(tag.*) FROM post_to_tag
           JOIN tag ON post_to_tag.tag = tag.id
           WHERE post_to_tag.post = post.id),
           '{}') "tags",
        coalesce (
          (SELECT array_agg(row(comment.*, author.*)) FROM comment
           JOIN author ON comment.author = author.id
           AND comment.parent = post.id), '{}') "comments"
    FROM post
    WHERE post.id = ?
    LIMIT 1 |] pid)
  return $! case ma of
    Just (post,authors,tags,comments) -> Just $!
      Article
        (fromRow post)
        (fromRows authors)
        (fromRows tags) $!
        PageSrc pid 0 (fromRows comments)
    Nothing -> Nothing

--------------------------------------------------------------------------------
-- Pages
--------------------------------------------------------------------------------

data MaybeId :: Maybe * -> * where
  JustId :: Id a -> MaybeId (Just a)
  NoId   :: MaybeId Nothing

class Pages p a where
  pageAt
    :: MaybeId p -- ^ parent (maybe)
    -> Int32     -- ^ page offset
    -> Int32     -- ^ page size
    -> Tx Postgres s (Vector a)

instance Pages (Just Post) Comment where
  pageAt (JustId src) offset size =
    vectorEx $
      [stmt|SELECT * FROM comment WHERE parent = ? ORDER BY id
            OFFSET ? LIMIT ?
           |] src offset size

instance Pages Nothing Post where
  pageAt _NoId offset size =
    vectorEx $
      [stmt|SELECT * FROM post ORDER BY id OFFSET ? LIMIT ? |] offset size

--------------------------------------------------------------------------------
-- Summaries
--------------------------------------------------------------------------------

instance Pages (Just Post) Summary where
  pageAt (JustId (Id pid)) offset size =
    let
      s2 :: Int32
      s2 = size `div` 2
    in vectorEx
      ([stmt|
       SELECT
         row(post.id,post.feature,post.title,
             summary.content,post.created,post.updated) "post",

        coalesce (
          (SELECT array_agg(author.*) FROM post_to_author
           JOIN author ON post_to_author.author = author.id
           WHERE post_to_author.post = post.id),
          '{}') "authors",

        coalesce (
          (SELECT array_agg(tag.*) FROM post_to_tag
           JOIN tag ON post_to_tag.tag = tag.id
           WHERE post_to_tag.post = post.id),
           '{}') "tags"

       FROM summary JOIN post ON summary.post = post.id
       WHERE post.id <= ?
       ORDER BY post.id DESC
       OFFSET ?
       LIMIT ?
       |] (pid+3) offset size)

instance Pages Nothing Summary where
  pageAt _NoId offset size =
    vectorEx
      ([stmt|
       SELECT
         row(post.id,post.feature,post.title,
             summary.content,post.created,post.updated) "post",

         coalesce (
           (SELECT array_agg(author.*) FROM post_to_author
            JOIN author ON post_to_author.author = author.id
            WHERE post_to_author.post = post.id),
           '{}') "authors",

         coalesce (
           (SELECT array_agg(tag.*) FROM post_to_tag
            JOIN tag ON post_to_tag.tag = tag.id
            WHERE post_to_tag.post = post.id),
            '{}') "tags"

       FROM summary JOIN post ON summary.post = post.id
       ORDER BY post.id DESC
       OFFSET ?
       LIMIT ?
       |] offset size)

--------------------------------------------------------------------------------
-- Search
--------------------------------------------------------------------------------

postsSearch
  :: Text
  -> Int32
  -> Int32
  -> Tx Postgres s (Vector Summary)
postsSearch terms offset size =
  vectorEx $ [stmt|
    SELECT
      row(post.id,post.feature,post.title,
          ts_headline(summary.content,
                      to_tsquery('english',?),
                      'HighlightAll=true,FragmentDelimiter=""'),
          post.created,
          post.updated) "post",
      coalesce (
        (SELECT array_agg(author.*) FROM post_to_author
         JOIN author ON post_to_author.author = author.id
         WHERE post_to_author.post = post.id),
        '{}') "authors",
      coalesce (
        (SELECT array_agg(tag.*) FROM post_to_tag
         JOIN tag ON post_to_tag.tag = tag.id
         WHERE post_to_tag.post = post.id),
        '{}') "tags"
    FROM post_search
      JOIN summary ON summary.post = post_search.post
      JOIN post    ON post.id      = post_search.post
    WHERE post_search.document @@ to_tsquery('english',?)
    GROUP BY post.id, summary.post
    ORDER BY post.id DESC
    OFFSET ?
    LIMIT ? |]
    terms
    terms
    offset
    size

tagSearch
  :: Id Post
  -> [Int32]
  -> Int32
  -> Int32
  -> Tx Postgres s (Vector Summary)
tagSearch (Id pid) terms offset size = do
  vectorEx $ [stmt|
    SELECT
      row(post.id, post.feature, post.title,
          summary.content, post.created, post.updated) "post",
      coalesce (
        (SELECT array_agg(author.*) FROM post_to_author
         JOIN author ON post_to_author.author = author.id
         WHERE post_to_author.post = post.id),
        '{}') "authors",
      coalesce (
        (SELECT array_agg(tag.*) FROM post_to_tag
         JOIN tag ON post_to_tag.tag = tag.id
         WHERE post_to_tag.post = post.id),
        '{}') "tags"
    FROM post
      JOIN summary     ON summary.post     = post.id
      JOIN post_to_tag ON post_to_tag.post = post.id
      JOIN tag         ON post_to_tag.tag  = tag.id
    WHERE post.id <= ? AND ARRAY[tag.id] <@ ?
    GROUP BY post.id, summary.post
    ORDER BY post.id DESC
    OFFSET ?
    LIMIT ? |]
    (let p3 = pid+3 in max pid p3)
    terms
    offset
    size

--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------

fromRows :: Rows a -> Vector a
fromRows = coerce

fromRow :: Row a -> a
fromRow = coerce

defaultId :: Id a
defaultId = Id (-1)

type family Result f where
  Result (a -> b) = Result b
  Result r = r

-- | Just a stupid syntactic thing
create :: forall f s. Schema (Result f) => f -> Tx Postgres s ()
create _ = schema (Nothing :: Maybe (Result f))

setSchema :: Tx Postgres s ()
setSchema = do
  create Tag
  create Author
  create Post
  create Comment

trydb :: Pool Postgres -> (forall s. Tx Postgres s a) -> IO (Maybe a)
trydb db t = do
  e <- session db (tx txMode t)
  case e of
    Right a -> return (Just a)
    Left  a -> Nothing <$ print a
