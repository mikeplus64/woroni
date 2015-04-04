{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module DB.Query where
import           Control.Applicative
import           Control.Monad
import           Data.Coerce
import           Data.Maybe
import           Data.Text
import qualified Data.Text.Lazy      as L
import           Data.Time

import Database.PostgreSQL.Simple.SqlQQ
import Snap.Snaplet.PostgresqlSimple

import Text.Blaze.Html               (toHtml)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import DB.Schema

prepare :: Text -> L.Text
prepare = renderHtml . toHtml

-- | Add an author (for a post or comment)
addAuthor :: HasPostgres m => Name -> Inet -> Maybe Email -> m (Maybe (Id Author))
addAuthor name addr em = do
  aidIns <- (fmap fromOnly . listToMaybe) `liftM` query
    [sql|INSERT INTO author VALUES(default,?,?,?) RETURNING author.id|]
    (addr, prepare name, liftM prepare em)
  case aidIns of
    Just aid -> return (Just aid)
    Nothing  -> (fmap fromOnly . listToMaybe) `liftM` query
                [sql|SELECT author.id FROM author
                     WHERE author.name = ? |] (Only name)

addTag :: HasPostgres m => Text -> m (Maybe (Id Tag))
addTag t = (fmap fromOnly . listToMaybe) `liftM` query
  [sql|INSERT INTO tag VALUES(default,?) RETURNING tag.id|] (Only t)

--------------------------------------------------------------------------------
-- Post queries

getComments :: Query
getComments = [sql|SELECT comment.* FROM comment WHERE comment.parent=?
                   ORDER BY comment.created |]

getNewestPostId :: HasPostgres m => m (Id Post)
getNewestPostId = do
  [Only p] <- query_ [sql|SELECT post.id FROM post ORDER BY post.created DESC LIMIT 1|]
  return p

getCommentAuthor :: HasPostgres m => Id Comment -> m (Maybe Author)
getCommentAuthor cid = listToMaybe `liftM` query
  [sql|SELECT author.*
       FROM comment JOIN author ON comment.author = author.id
       WHERE comment.id=? |] (Only cid)

getTags :: HasPostgres m => Id Post -> m [Tag]
getTags = query
  [sql|SELECT tag.*
       FROM post_to_tag JOIN tag ON post_to_tag.tag = tag.id
       WHERE post_to_tag.post = ? |] . Only

getTagNames :: HasPostgres m => [Id Tag] -> m [Text]
getTagNames = liftM (coerce :: [Only Text] -> [Text]) . query
  [sql| SELECT tag.name FROM tag WHERE tag.id IN ? |] . Only . In

getAuthorNames :: HasPostgres m => [Id Author] -> m [Text]
getAuthorNames = liftM (coerce :: [Only Text] -> [Text]) . query
  [sql| SELECT author.name FROM author WHERE author.id IN ? |] . Only . In

getAuthors :: HasPostgres m => Id Post -> m [Author]
getAuthors = query
  [sql|SELECT author.*
       FROM post_to_author JOIN author ON post_to_author.author = author.id
       WHERE post_to_author.post=? |] . Only

getPostsByTags :: HasPostgres m => [Id Tag] -> m [Post]
getPostsByTags = query [sql|SELECT * FROM post WHERE post.tag IN ?|] . Only
                 . In

getPostsByDateRange :: HasPostgres m => (UTCTime, UTCTime) -> m [Post]
getPostsByDateRange = query
  [sql|SELECT * FROM post WHERE post.created >= ? AND post.created < ?|]

getPostsNewer, getPostsOlder :: HasPostgres m => UTCTime -> m [Post]
getPostsNewer = query [sql|SELECT * FROM post WHERE post.created >= ?|].Only
getPostsOlder = query [sql|SELECT * FROM post WHERE post.created < ?|].Only

--------------------------------------------------------------------------------
-- Misc

getAllTags :: HasPostgres m => m [Tag]
getAllTags = query_ [sql|SELECT * FROM tag|]

getAllAuthors :: HasPostgres m => m [Tag]
getAllAuthors = query_ [sql|SELECT * FROM author, post_to_author
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
  :: HasPostgres m
  => CommentAuthor
  -> Id Post -- ^ the post
  -> Text    -- ^ content
  -> m Bool
addComment cauthor parent content =
  case cauthor of
    Known aid ->
      (1 ==) `liftM` execute
        [sql|INSERT INTO comment VALUES(default,?,?,?,default,null)|]
        (parent, aid, prepare content)
    Named addr name -> do
      -- this calls prepare
      aid' <- addAuthor name addr Nothing
      case aid' of
        Just aid -> addComment (Known aid) parent content
        Nothing  -> return False
    Anonymous addr -> do
      -- this calls prepare
      aid' <- addAuthor "Anonymous" addr Nothing
      case aid' of
        Just aid -> addComment (Known aid) parent content
        Nothing  -> return False

--------------------------------------------------------------------------------
-- Post authoring

addPost :: HasPostgres m => [Id Tag] -> [Id Author] -> Text -> Text -> m (Id Post)
addPost tagIds authorIds title content = do
  [Only pid] <- query
    [sql|INSERT INTO post
         VALUES (default,false,null,?,?,default,null)
         RETURNING post.id|] (title,content)

  forM_ tagIds $ \tid -> execute
    [sql|INSERT INTO post_to_tag VALUES(?, ?)|] (pid, tid)

  forM_ authorIds $ \aid -> execute
    [sql|INSERT INTO post_to_author VALUES(?, ?)|] (pid, aid)

  return pid

