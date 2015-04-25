{-# LANGUAGE BangPatterns, GADTs, NamedFieldPuns, NoMonoLocalBinds,
             OverloadedStrings, QuasiQuotes, RankNTypes, RecordWildCards,
             ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Woroni.Template where
--------------------------------------------------------------------------------
import Heist
import Heist.Compiled
--------------------------------------------------------------------------------
import           Data.Vector ((!))
import qualified Data.Vector as V
--------------------------------------------------------------------------------
import Woroni.Application
import Woroni.Backend.Types
import Woroni.Prelude
--------------------------------------------------------------------------------

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
          (summarySplice id (postId . post . fst <$> postRS))
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
      (summarySplice id (return (-1)))
      (lift (view page) >>= \p -> case p of
        Search _ (Page _ vs) -> return (V.toList vs)
        _                    -> fail "I don't know")


commentList :: (Article, a) -> [AComment]
commentList (p, _) = case postComments p of
  PageSrc _ _ v -> V.toList v

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

{-# INLINE summarySplice #-}
summarySplice
  :: forall a. (a -> Summary) -> RuntimeSplice H Id'
  -> Splices (RuntimeSplice H a -> Splice H)
summarySplice getSummary getSelected = do
  let
    {-# INLINE postS #-}
    postS :: (Post -> Text) -> RuntimeSplice H a -> Splice H
    postS g = pureText (g . fromRow . summaryPost . getSummary)

  "summary-id"   ## postS (formatId' . postId)
  "summary-href" ## postS (("/post/" <>) . formatId' . postId)
  "summary-sel"  ## \summaryRS -> do
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

formatTags :: Vector Tag -> Text
formatTags = oxfordCommas formatTag

formatPostTags :: Vector Tag -> Text
formatPostTags = oxfordCommas formatPostTag

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
