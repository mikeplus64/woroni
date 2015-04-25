{-# LANGUAGE BangPatterns, GADTs, NamedFieldPuns, NoMonoLocalBinds,
             OverloadedStrings, QuasiQuotes, RankNTypes, RecordWildCards,
             ScopedTypeVariables #-}
module Woroni.Template where
--------------------------------------------------------------------------------
import Heist
import Heist.Compiled
import Snap
--------------------------------------------------------------------------------
import           Data.Vector ((!))
import qualified Data.Vector as V
--------------------------------------------------------------------------------
import Debug.Trace
import Woroni.Application
import Woroni.Backend.Types
import Woroni.Prelude

templates :: SpliceConfig H
templates = mempty & scCompiledSplices  .~ splices
                   & scAttributeSplices .~ attrs

attrs :: Splices (AttrSplice H)
attrs =
  "search-terms" ## \_ ->
    lift (view page) <&> \p -> case p of
      Search terms _ -> [("value", terms)]
      _              -> []


-- | Main splices
splices :: Splices (Splice H)
splices = do
  -- Post splices
  "post" ## deferMap return
    (withSplices runChildren $ do
        let splicePost toText = pureText (toText . post . fst)
        "post-id"       ## splicePost (formatId' . postId)
        "post-content"  ## splicePost postContent
        "page-title"    ## splicePost postTitle
        "post-times"    ## splicePost (\p -> formatTimes
                                             (postCreated p)
                                             (postUpdated p))
        "post-authors"  ## pureText (formatAuthors . postAuthors . fst)
        "post-tags"     ## pureText (formatPostTags . postTags . fst)
        "post-comments" ## \postRS -> manyWithSplices
          (callTemplate "comment")
          (commentSplice pureText  )
          (commentList <$> postRS)
        "post-aside" ## \postRS -> manyWithSplices
          (callTemplate "summary")
          (summarySplice id (postId . fst <$> postRS))
          (V.toList . snd <$> postRS))

    (lift (view page) >>= \p -> case p of
      PostView a s -> return (a, s)
      _            -> fail "Not a PostView")

  -- Searches
  "search-title" ## pureText id $
    lift (view page) >>= \p -> case p of
      Search terms _ -> return terms
      _              -> fail "No search title"

  "post-summaries" ##
    manyWithSplices
      (callTemplate "summary")
      (summarySplice id undefined)
      (lift (view page) >>= \p -> case p of
        Search terms (Page _ vs) -> return (V.toList vs)
        _                        -> fail "I don't know")


{-
  manyWithSplices
    (callTemplate "summary")
    (do fmap snd summarySplice
        "page-title" ## pureText _
        "summary" ## \rs -> do
          content <- runChildren
          tag     <- pureText (uncurry formatSummaryA) $
            (,) Nothing
            <$> fmap (postId . fromRow . summaryPost . snd) rs
          return (tag <> content <> yieldPureText "</a>"))
-}

commentList :: (Article, a) -> [AComment]
commentList (p, _) = case postComments p of
  PageSrc _ _ v -> V.toList v
  _             -> []

summaryList :: (a, Vector Summary) -> [Summary]
summaryList (_, v) = V.toList v

--------------------------------------------------------------------------------
-- Comment splices

commentSplice :: ((AComment -> Text) -> v) -> Splices v
commentSplice f = do
  "comment-id"      ## f (formatId' . acommentId)
  "comment-content" ## f acommentContent
  "comment-author"  ## f (\a -> formatAuthor (cauthorId a) (cauthorName a))
  "comment-times"   ## f (\c -> formatTimes (acommentCreated c) Nothing)

--------------------------------------------------------------------------------
-- Summary

summarySplice
  :: forall a. (a -> Summary) -> RuntimeSplice H Id'
  -> Splices (RuntimeSplice H a -> Splice H)
summarySplice getSummary getSelected = do
  let
    {-# INLINE postS #-}
    postS :: (Post -> Text) -> RuntimeSplice H a -> Splice H
    postS g = pureText (g . fromRow . summaryPost . getSummary)

  "summary-id"      ## postS (formatId' . postId)
  "summary-href"    ## postS (("/post/" <>) . formatId' . postId)
  "summary-content" ## postS postContent
  "summary-title"   ## postS postTitle
  "summary-times"   ## postS (\p -> formatTimes (postCreated p) (postUpdated p))
  "summary-authors" ## pureText
    (oxfordCommas authorName . fromRows . summaryAuthors . getSummary)

  "summary-tags"    ## pureText
    (oxfordCommas tagName . fromRows . summaryTags . getSummary)

{-
summaries :: Splice H
summaries =
  manyWithSplices
    (callTemplate "summary")
    (do let spliceSummary f = pureSplice (textSplice f)
        "summary"         ## \summaryR -> do
          contents    <- runChildren
          isSelectedA <- pureSplice (textSplice formatSummaryA) $ do
            selected  <- lift . preview $
              page . (_Search . to snd . to aggTop . _Just `failing`
                      _PostView . to postId)
            sr <- summaryR
            return (selected, sr)
          return (isSelectedA <> contents <> yieldPureText "</a>")
        "summary-date"    ## spliceSummary (formatTimes . postTimes)
        "summary-image"   ## spliceSummary (Data.Foldable.fold . postImage)
        "summary-id"      ## spliceSummary (formatId . postId)
        "summary-content" ## spliceSummary postContent
        "summary-title"   ## spliceSummary postTitle
        "summary-times"   ## spliceSummary (formatTimes . postTimes)
        "summary-authors" ## spliceSummary (formatSummaryAuthors . postAuthors)
        "summary-tags"    ## spliceSummary (formatSummaryTags . postTags))

    (getSummariesFromView)

asideAggregate :: Id Schema.Post -> Aggregate Schema.Post
asideAggregate pid = Aggregate
  { aggLimit   = 10
  , aggSize    = 140
  , aggTop     = Just pid
  , aggTags    = Nothing
  , aggAuthors = Nothing
  , aggTerms   = Nothing
  }

getSummariesFromView :: RuntimeSplice H [Summary]
getSummariesFromView = do
  p <- lift (view page)
  case p of
    -- if we're on a page, we want to get summaries of articles
    -- that are "around" that article
    PostView post ->
      lift $ getSummaries (asideAggregate (postId post))

    -- otherwise, just get the most recent ones
    Home        ->  lift $
      getSummaries Aggregate
        { aggLimit   = 10
        , aggSize    = 320
        , aggTop     = Nothing
        , aggTags    = Nothing
        , aggAuthors = Nothing
        , aggTerms   = Nothing
        }
    -- or, oh boy, this is exciting ...
    Search _ agg  -> lift (getSummaries agg)
    _             -> return []

fromView :: Traversal' View a -> RuntimeSplice H a
fromView v = do
  p <- lift (preview (page.v))
  case p of
    Just p' -> return p'
    Nothing -> fail "Template.fromView: Can't get that view."

splices :: Splices (Splice H)
splices = do
  ------------------------------------------------------------------------------
  -- Post splices
  "post" ## deferMap return
    (withSplices runChildren
      (do let splicePost toText = pureSplice (textSplice toText)
          "post-id"       ## splicePost (formatId . postId)
          "post-image"    ## splicePost (Data.Foldable.fold . postImage)
          "post-content"  ## splicePost postContent
          "page-title"    ## splicePost postTitle
          "post-times"    ## splicePost (formatTimes . postTimes)
          "post-authors"  ## splicePost (formatAuthors . postAuthors)
          "post-tags"     ## splicePost (formatPostTags . postTags)
          "post-comments" ## \rp -> manyWithSplices
            (callTemplate "comment")
            (commentBy pureText)
            (fmap (commentList . postComments) rp)
      )
    )

    (fromView _PostView)

  ------------------------------------------------------------------------------
  -- Comment splices for the "Comment" view - only used at /comment/:id
  commentBy (\f -> spliceFrom f _CommentView)

  ------------------------------------------------------------------------------
  -- Summaries
  "post-summaries" ## summaries

  "search-name" ## pureSplice (textSplice id) $
    Data.Foldable.fold `fmap` lift (preview (page . _Search . to fst))

  ------------------------------------------------------------------------------
  -- Misc
  "all-tags" ## deferMany
    (pureSplice (textSplice formatTag))
    (liftIO . readIORef =<< lift (view allTags))

-}

--------------------------------------------------------------------------------
-- Formatters

formatId :: Id a -> Text
formatId = fromString . show . fromId

formatId' :: Int32 -> Text
formatId' = fromString . show

formatTimes :: UTCTime -> Maybe UTCTime -> Text
formatTimes c _ = fromString (formatTime defaultTimeLocale "%B %d, %Y" c)

formatAuthor :: Id' -> Text -> Text
formatAuthor aid name =
  "<a class='author' href='/author/"
  <> formatId' aid <> "'>"
  <> name
  <> "</a>"

formatAuthors :: Vector Author -> Text
formatAuthors = oxfordCommas (\a -> formatAuthor (authorId a) (authorName a))

formatSummaryAuthor :: Author -> Text
formatSummaryAuthor Author{..} =
  "<span class='summary author'>" <> authorName <> "</span>"

formatSummaryAuthors :: Vector Author -> Text
formatSummaryAuthors = oxfordCommas formatSummaryAuthor

formatTags :: Vector Tag -> Text
formatTags = oxfordCommas formatTag

formatPostTags :: Vector Tag -> Text
formatPostTags = oxfordCommas formatPostTag

formatSummaryTags :: Vector Tag -> Text
formatSummaryTags = oxfordCommas formatSummaryTag

{-# INLINE formatSummaryA #-}
formatSummaryA :: Maybe Id' -> Id' -> Text
formatSummaryA sel sid =
  if Just sid == sel
  then "<a href='/post/" <> formatId' sid <> "' class='summary selected'>"
  else "<a href='/post/" <> formatId' sid <> "' class='summary'>"

formatTag :: Tag -> Text
formatTag tag =
  "<a onclick='update_aside(this); return false;' class='tag selected' "
  <> "href='/tag/"
  <> idBS
  <> "'>"
  <> "<span class='tag id' style='display:none'>" <> idBS <> "</span>"
  <> tagName tag
  <> "</a>"
 where
  idBS = formatId' (tagId tag)

formatPostTag :: Tag -> Text
formatPostTag tag =
  "<a class='tag' "
  <> "href='/tag/"
  <> formatId' (tagId tag)
  <> "'>"
  <> tagName tag
  <> "</a>"

formatSummaryTag :: Tag -> Text
formatSummaryTag tag = "<span class='summary tag'>" <> tagName tag <> "</span>"

oxfordCommas :: (a -> Text) -> Vector a -> Text
oxfordCommas fmt v = case V.length v of
  0 -> "none"
  1 -> fmt (v!0)
  2 -> fmt (v!0) <> " and " <> fmt (v!1)
  n -> loop 1 (fmt (v!0))
 where
  loop !ix !acc
    | ix < V.length v-1 = loop (ix+1) (acc <> ", " <> fmt (v!ix))
    | otherwise         = acc <> ", and " <> fmt (v!ix)

--------------------------------------------------------------------------------
-- Util

pureText :: Monad n => (a -> Text) -> RuntimeSplice n a -> Splice n
pureText = pureSplice . textSplice
