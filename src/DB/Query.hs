{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module DB.Query where
import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import           Data.Text
import qualified Data.Text.Lazy                   as L
import           Data.Time
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ

import Text.Blaze.Html               (toHtml)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import DB.Schema

prepare :: Text -> L.Text
prepare = renderHtml . toHtml

-- | Add an author (for a post or comment)
addAuthor :: Connection -> Name -> Inet -> Maybe Email -> IO (Maybe (Id Author))
addAuthor conn name addr em = do
  aidIns <- fmap fromOnly . listToMaybe <$> query conn
    [sql|INSERT INTO author VALUES(default,?,?,?) RETURNING author.id|]
    (addr, prepare name, fmap prepare em)
  case aidIns of
    Just aid -> return (Just aid)
    Nothing  -> fmap fromOnly . listToMaybe <$> query conn
                [sql|SELECT author.id FROM author
                     WHERE author.name = ? |] (Only name)

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
getPostsByTags c = query c [sql|SELECT * FROM post WHERE post.tag IN ?|] . Only
                   . In

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

createAuthor :: Maybe Name -> Inet -> Maybe Email -> CommentAuthor
createAuthor (Just n) i _ = Named i n
createAuthor Nothing  i _ = Anonymous i

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
        (parent, aid, prepare content)

    Named addr name -> do
      -- this calls prepare
      aid' <- addAuthor conn name addr Nothing
      case aid' of
        Just aid -> addComment conn (Known aid) parent content
        Nothing  -> return False

    Anonymous addr -> do
      -- this calls prepare
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

  forM_ tagIds $ \tid ->
    execute con
    [sql|INSERT INTO post_to_tag VALUES(?, ?)|]
    (pid, tid)

  forM_ authorIds $ \aid ->
    execute con
    [sql|INSERT INTO post_to_author VALUES(?, ?)|]
    (pid, aid)

  return pid


