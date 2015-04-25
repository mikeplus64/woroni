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
  , postsBy
  , postsTagged
  , postsTaggedBy
  , postsSearch
    -- * Utility
  , getPool
  , setSchema
  , txMode
  , backend
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
  insert   :: a -> Tx Postgres s (Id a)
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
  { acommentId       :: {-# UNPACK #-} !Id'
  , acommentLocalId  :: {-# UNPACK #-} !Id'
  , acommentParent   :: {-# UNPACK #-} !Id'
  , acommentContent  :: !Text
  , acommentCreated  :: !UTCTime
  , acommentAuthorId :: !Id'
  , acommentAddress  :: !ByteString
  , cauthorId        :: {-# UNPACK #-} !Id'
  , cauthorName      :: !Text
  , cauthorEmail     :: !(Maybe Text)
  } deriving (Show,Eq,Generic)

data Tag = Tag
  { tagId   :: {-# UNPACK #-} !Id'
  , tagName :: !Text
  } deriving (Show,Eq,Generic)

data Author = Author
  { authorId    :: {-# UNPACK #-} !Id'
  , authorName  :: !Text
  , authorEmail :: !(Maybe Text)
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

  insert (Tag _ n) = fmap one . singleEx $
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
      )|]
    unitEx [stmt|INSERT INTO author (id, name, email)
                 SELECT 0, 'Anonymous', NULL
                 WHERE NOT EXISTS (SELECT 1 FROM author WHERE id = 0)|]

  lookupId = maybeEx . [stmt|SELECT * FROM author WHERE author.id = ? LIMIT 1|]

  insert Author{..} =
    if authorId > 0
    then Id authorId <$ unitEx updateAuthor
    else one <$> singleEx insertAuthor
   where
    insertAuthor :: Stmt Postgres
    insertAuthor =
      [stmt|INSERT INTO author VALUES(default,?,?) RETURNING id|]
      authorName authorEmail

    updateAuthor :: Stmt Postgres
    updateAuthor =
      [stmt|UPDATE author SET name = ?, email = ?
            WHERE author.id = ? |]
      authorName authorEmail authorId

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
updatePost pid title content date = do
  let md = renderMarkdown content
  unitEx $ [stmt|UPDATE post SET title = ?, content = ?, updated = ?
                 WHERE id = ?|]
    title
    md
    date
    pid

  unitEx $ [stmt|UPDATE summary SET content = ? WHERE post = ?|]
    (summarise id md)
    pid

  unitEx $ [stmt|UPDATE post_original_markdown SET markdown = ? WHERE post = ?|]
    content
    pid

newPost
  :: [Id Author] -- ^ Authors
  -> [Id Tag]    -- ^ Tags
  -> Bool        -- ^ Whether the post is a "feature" or not
  -> Text        -- ^ Title
  -> Text        -- ^ Markdown content
  -> UTCTime     -- ^ Insert date
  -> Tx Postgres s (Id Post)
newPost authors tags feature title content now = do
  when (null tags) (fail "newPost: post must have tags")
  pid <- insert Post
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
  unitEx $
    [stmt|INSERT INTO post_original_markdown VALUES(?,?)|] pid content
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
      CREATE OR REPLACE FUNCTION post_document(md post_original_markdown)
          RETURNS tsvector AS $$
        SELECT to_tsvector('english',
          string_agg(tag.name, ' ') || ' ' ||
          string_agg(author.name, ' ') || ' ' ||
          post.title || ' ' ||
          md.markdown )
        FROM post
          JOIN post_to_author ON post_to_author.post = md.post
          JOIN author         ON author.id = post_to_author.author
          JOIN post_to_tag ON post_to_tag.post = md.post
          JOIN tag         ON tag.id = post_to_tag.tag
        WHERE post.id = md.post
        GROUP BY post.id
      $$ LANGUAGE 'sql' STABLE |]
    -- Search update/insert rules; delete is handled already
    -- Note we can't just make Postgres rules for summaries since we need to
    -- render parse the HTML somehow
    unitEx [stmt|
      CREATE OR REPLACE RULE search_ins AS ON INSERT
          TO post_original_markdown DO
        INSERT INTO post_search VALUES(NEW.post, (SELECT post_document(NEW)))|]

    unitEx [stmt|
      CREATE OR REPLACE RULE search_upd AS ON UPDATE
          TO post_original_markdown DO
        UPDATE post_search SET document = (SELECT post_document(NEW))
        WHERE post_search.post = NEW.post |]

  lookupId = maybeEx . [stmt|SELECT * FROM post WHERE post.id = ? LIMIT 1|]

  insert Post{..} =
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
    Just n -> insert Author
      { authorId    = -1
      , authorName  = n
      , authorEmail = email
      }
    Nothing -> return (Id 0)
  insert $ Comment
    { commentId      = -1
    , commentLocalId = -1
    , commentParent  = fromId pid
    , commentContent = renderMarkdown content
    , commentCreated = now
    , commentAuthorId  = fromId aid
    , commentAddress = addr
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

  insert Comment{..} =
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
        row(post.*)          "post",
        array_agg(author.*)  "authors",
        array_agg(tag.*)     "tags",
        coalesce (
         (SELECT array_agg(row(comment.*, author.*))
          FROM comment JOIN author ON comment.author = author.id
          WHERE comment.parent = post.id), '{}') "comments"
      FROM post
        JOIN post_to_author ON post_to_author.post = post.id
        JOIN author         ON author.id = post_to_author.author
        JOIN post_to_tag ON post_to_tag.post = post.id
        JOIN tag         ON tag.id = post_to_tag.tag
      WHERE post.id = ?
      GROUP BY post.id |]
    pid )
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
         array_agg(author.*)  "authors",
         array_agg(tag.*)     "tags"
       FROM summary JOIN post ON summary.post = post.id
         JOIN post_to_author ON post_to_author.post = post.id
         JOIN author         ON author.id = post_to_author.author
         JOIN post_to_tag ON post_to_tag.post = post.id
         JOIN tag         ON tag.id = post_to_tag.tag
       WHERE post.id >= ?
       GROUP BY summary.post, post.id
       ORDER BY post.id DESC
       OFFSET ?
       LIMIT ?
       |] (pid-s2) offset size)

instance Pages Nothing Summary where
  pageAt _NoId offset size =
    vectorEx
      ([stmt|
       SELECT
         row(post.id,post.feature,post.title,
             summary.content,post.created,post.updated) "post",
         array_agg(author.*)  "authors",
         array_agg(tag.*)     "tags"
       FROM summary JOIN post ON summary.post = post.id
         JOIN post_to_author ON post_to_author.post = post.id
         JOIN author         ON author.id = post_to_author.author
         JOIN post_to_tag ON post_to_tag.post = post.id
         JOIN tag         ON tag.id = post_to_tag.tag
       GROUP BY summary.post, post.id
       ORDER BY post.id DESC
       OFFSET ?
       LIMIT ?
       |] offset size)

--------------------------------------------------------------------------------
-- Search
--------------------------------------------------------------------------------

postsBy :: [Id Author] -> Int32 -> Int32 -> Tx Postgres s (Vector Summary)
postsBy authors offset size =
  vectorEx $
    [stmt|
      SELECT
        row(post.id,post.feature,post.title,
            summary.content,post.created,post.updated) "post",
        array_agg(author.*)  "authors",
        array_agg(tag.*)     "tags"
      FROM summary JOIN post ON summary.post = post.id
        JOIN post_to_author ON post_to_author.post = post.id
        JOIN author ON author.id = post_to_author.author AND author.id = ANY(?)
        JOIN post_to_tag ON post_to_tag.post = post.id
        JOIN tag         ON tag.id = post_to_tag.tag
      GROUP BY summary.post, post.id
      ORDER BY post.id DESC
      OFFSET ?
      LIMIT ? |]
    (coerce authors :: [Int32])
    offset
    size

postsTagged :: [Id Tag] -> Int32 -> Int32 -> Tx Postgres s (Vector Summary)
postsTagged tags offset size =
  vectorEx $
    [stmt|
      SELECT
        row(post.id,post.feature,post.title,
            summary.content,post.created,post.updated) "post",
        array_agg(author.*)  "authors",
        array_agg(tag.*)     "tags"
      FROM summary JOIN post ON summary.post = post.id
        JOIN post_to_tag ON post_to_tag.post = post.id
        JOIN tag         ON tag.id = post_to_tag.tag AND tag.id = ANY(?)
        JOIN post_to_author ON post_to_author.post = post.id
        JOIN author         ON author.id = post_to_author.author
      ORDER BY post.id DESC
      OFFSET ?
      LIMIT ? |]
    (coerce tags :: [Int32])
    offset
    size

postsTaggedBy
  :: [Id Author]
  -> [Id Tag]
  -> Int32
  -> Int32
  -> Tx Postgres s (Vector Summary)
postsTaggedBy authors tags offset size =
  let na = null authors
      nt = null tags
  in if
    | na && nt  -> pageAt NoId offset size
    | na        -> postsTagged tags offset size
    | nt        -> postsBy authors offset size
    | otherwise -> vectorEx $ [stmt|
        SELECT
          row(post.id,post.feature,post.title,
              summary.content,post.created,post.updated) "post",
          array_agg(author.*)  "authors",
          array_agg(tag.*)     "tags"
        FROM summary JOIN post ON summary.post = post.id
          JOIN post_to_tag ON post_to_tag.post = post.id
          JOIN tag         ON tag.id = post_to_tag.tag AND tag.id = ANY(?)
          JOIN post_to_author ON post_to_author.post = post.id
          JOIN author ON author.id = post_to_author.author
                     AND author.id = ANY(?)
        ORDER BY post.id DESC
        OFFSET ?
        LIMIT ? |]
      (coerce tags :: [Int32])
      (coerce authors :: [Int32])
      offset
      size

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
      array_agg(author.*) "authors",
      array_agg(tag.*)    "tags"
    FROM post_search
      JOIN summary ON summary.post = post_search.post
      JOIN post    ON post.id      = post_search.post
      JOIN post_to_tag ON post_to_tag.post = post.id
      JOIN tag         ON tag.id           = post_to_tag.tag
      JOIN post_to_author ON post_to_author.post = post.id
      JOIN author         ON author.id           = post_to_author.author
    WHERE post_search.document @@ to_tsquery('english',?)
    GROUP BY post.id, summary.post
    ORDER BY post.id DESC
    OFFSET ?
    LIMIT ? |]
    terms
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

backend :: Pool Postgres -> (forall s. Tx Postgres s a) -> IO (Maybe a)
backend db t = do
  e <- session db (tx txMode t)
  case e of
    Right a -> return (Just a)
    Left  a -> Nothing <$ print a
