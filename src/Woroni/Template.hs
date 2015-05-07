{-# LANGUAGE BangPatterns, GADTs, NamedFieldPuns, NoMonoLocalBinds,
             OverloadedStrings, QuasiQuotes, RankNTypes, RecordWildCards,
             ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Woroni.Template where
--------------------------------------------------------------------------------
import Heist
import Heist.Compiled
import Heist.Internal.Types (HeistState (..))
--------------------------------------------------------------------------------
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as L
import qualified Data.Text.Lazy.Builder     as B
import qualified Data.Text.Lazy.Builder.Int as B
import           Data.Vector                ((!))
import qualified Data.Vector                as V
--------------------------------------------------------------------------------
import Woroni.Application
import Woroni.Backend.Types
import Woroni.Prelude
--------------------------------------------------------------------------------

templates :: SpliceConfig H
templates = mempty & scCompiledSplices  .~ splices

-- | Main splices
splices :: Splices (Splice H)
splices = do
  -- Post splices
  "post" ## deferMap return
    (withSplices runChildren $ do
        let splicePost toText = pureText (toText . post . fst)
        "post-id"       ## splicePost (formatId . postId)
        "post-content"  ## splicePost postContent
        "page-title"    ## splicePost postTitle
        "post-times"    ## splicePost (\p -> formatTimes
                                             (postCreated p)
                                             (postUpdated p))
        "post-authors"  ## pureText (formatAuthors . postAuthors . fst)
        "post-tags"     ## pureText (oxfordCommas formatPostTag . postTags . fst)
        "post-comments" ## \postRS -> manyWithSplices
          (callTemplate "comment")
          (commentSplice pureText  )
          (commentList <$> postRS)
        "post-aside" ## \postRS -> manyWithSplices
          (callTemplate "summary")
          (summarySplice id (postId . post . fst <$> postRS))
          (V.toList . snd <$> postRS)
        "authors-plural" ## pureText
          (\(post', _) -> if V.length (postAuthors post') > 1
                          then "s"
                          else "")
        "authors-info" ## \postRS -> manyWithSplices
          (callTemplate "author")
          (authorInfo)
          (V.toList . postAuthors . fst <$> postRS))
    (lift (view page) >>= \p -> case p of
      PostView a s -> return (a, s)
      _            -> fail "Not a PostView")

  -- Searches
  "search-title" ## pureText id $
    lift (view page) >>= \p -> case p of
      Search terms _ _ -> return terms
      _                -> fail "No search title"

  "all-tags" ## pureText id $
    formatAllTags <$> (liftIO . readIORef =<< lift (view allTags))

  "related-posts" ## filteredSummaries "summary" id $ do
    p <- lift (view page)
    return $! case p of
      Search _ pid _ -> maybe (-1) id pid
      _              -> -1

  "post-summaries" ## filteredSummaries "summary" id (return (-1))

  "featured-posts" ## filteredSummaries "summary"
    (V.filter (postFeature . fromRow . summaryPost))
    (return (-1))

  "feed-items" ##
    localHS removeDocType (rssSummaries "feed-item" id (return (-1)))

  "feed-link"  ## return $ yieldPureText
    "<link> http://woroni.com.au/feed.xml </link>"

  "new-posts" ## filteredSummaries "summary"
    (V.filter (not . postFeature . fromRow . summaryPost))
    (return (-1))

removeDocType :: HeistState t -> HeistState t
removeDocType hs = hs { _doctypes = [] }

authorInfo :: Splices (RuntimeSplice H Author -> Splice H)
authorInfo = do
  "author-id"          ## pureText (formatId . authorId)
  "author-name"        ## pureText authorName
  "author-email"       ## pureText (fold . authorEmail)
  "author-description" ## pureText (fold . authorDescription)

filteredSummaries :: ByteString -> (Vector Summary -> Vector Summary)
                  -> RuntimeSplice H Id' -> Splice H
filteredSummaries template filterS getId = do
  manyWithSplices
    (callTemplate template)
    (summarySplice id getId)
    (lift (view page) >>= \p -> case p of
      Search _ _ (Page _ vs) -> return (V.toList (filterS vs))
      _                      -> fail "I don't know")

rssSummaries :: ByteString -> (Vector Summary -> Vector Summary)
                  -> RuntimeSplice H Id' -> Splice H
rssSummaries template filterS getId = do
  manyWithSplices
    (callTemplate template)
    (rssItem id getId)
    (lift (view page) >>= \p -> case p of
      Search _ _ (Page _ vs) -> return (V.toList (filterS vs))
      _                      -> fail "I don't know")

commentList :: (Article, a) -> [AComment]
commentList (p, _) = case postComments p of
  PageSrc _ _ v -> V.toList v

summaryList :: (a, Vector Summary) -> [Summary]
summaryList (_, v) = V.toList v

--------------------------------------------------------------------------------

tagSplice :: Splices (RuntimeSplice H Tag -> Splice H)
tagSplice = do
  "tag-id"   ## pureText (formatId . tagId)
  "tag-name" ## pureText tagName

commentSplice :: ((AComment -> Text) -> v) -> Splices v
commentSplice f = do
  "comment-id"      ## f (formatId . acommentId)
  "comment-content" ## f acommentContent
  "comment-author"  ## f (\a -> formatAuthor (cauthorId a) (cauthorName a))
  "comment-times"   ## f (\c -> formatTimes (acommentCreated c) Nothing)

--------------------------------------------------------------------------------
-- Summary

{-# INLINE summarySplice #-}
summarySplice
  :: forall a. (a -> Summary) -> RuntimeSplice H Id'
  -> Splices (RuntimeSplice H a -> Splice H)
summarySplice getSummary getSelected = do
  let
    {-# INLINE postS #-}
    postS :: (Post -> Text) -> RuntimeSplice H a -> Splice H
    postS g = pureText (g . fromRow . summaryPost . getSummary)
  "summary-id"  ## postS (formatId . postId)
  "summary-sel" ## \summaryRS ->
    pureText
      (\isSelected -> if isSelected then "selected" else "")
      ((==) <$> getSelected
            <*> fmap (postId . fromRow . summaryPost . getSummary) summaryRS)
  "summary-content" ## postS postContent
  "summary-title"   ## postS postTitle
  "summary-times"   ## postS (\p -> formatTimes (postCreated p) (postUpdated p))
  "summary-authors" ## pureText
    (oxfordCommas authorName . fromRows . summaryAuthors . getSummary)
  "summary-tags"    ## pureText
    (oxfordCommas tagName . fromRows . summaryTags . getSummary)

{-# INLINE rssItem #-}
rssItem
  :: forall a. (a -> Summary) -> RuntimeSplice H Id'
  -> Splices (RuntimeSplice H a -> Splice H)
rssItem getSummary getSelected = do
  let
    {-# INLINE postS #-}
    postS :: (Post -> Text) -> RuntimeSplice H a -> Splice H
    postS g = pureText (g . fromRow . summaryPost . getSummary)
  "summary-link"    ## postS (wrapPostLink . formatId . postId)
  "rss-description" ## postS (wrapCDATA . postContent)
  summarySplice getSummary getSelected
 where
  wrapCDATA a = "<![CDATA[" <> a <> "]]>"
  wrapPostLink a = "<link> http://localhost:8000/post/" <> a <> " </link>"

--------------------------------------------------------------------------------
-- Formatters

formatId :: Int32 -> Text
formatId = fromString . show

formatTimes :: UTCTime -> Maybe UTCTime -> Text
formatTimes c _ = fromString (formatTime defaultTimeLocale "%B %d, %Y" c)

formatAuthor :: Id' -> Text -> Text
formatAuthor aid name =
  "<a class='author' href='/author/"
  <> formatId aid <> "'>"
  <> name
  <> "</a>"

formatAuthors :: Vector Author -> Text
formatAuthors = oxfordCommas (\a -> formatAuthor (authorId a) (authorName a))

formatAllTags :: Vector Tag -> Text
formatAllTags =
  V.foldl'
    (\acc tag ->
      let tagIdB = B.decimal (tagId tag)
      in
      acc <>
      builder2text
        (B.fromText "<a class='tag selected' tag-id=" <> tagIdB
        <> B.fromText " href='/tag/" <> tagIdB
        <> B.fromText "' onclick=\"update_aside(this); return false;\">"
        <> B.fromText (tagName tag)
        <> B.fromText "</a>"))
    mempty

formatPostTag :: Tag -> Text
formatPostTag tag = T.concat
  [ "<a class='tag' "
  , "href='/tag/"
  , formatId (tagId tag)
  , "'>"
  , tagName tag
  , "</a>"
  ]

oxfordCommas :: (a -> Text) -> Vector a -> Text
oxfordCommas fmt v = case V.length v of
  0 -> "none"
  1 -> fmt (v!0)
  2 -> fmt (v!0) <> " and " <> fmt (v!1)
  _ -> go 1 (fmt (v!0))
 where
  go !i !acc
    | i < V.length v-1 = go (i+1) (acc <> ", " <> fmt (v!i))
    | otherwise        = acc <> ", and " <> fmt (v!i)

--------------------------------------------------------------------------------
-- Util

pureText :: Monad n => (a -> Text) -> RuntimeSplice n a -> Splice n
pureText = pureSplice . textSplice

builder2text :: B.Builder -> Text
builder2text = L.toStrict . B.toLazyText
