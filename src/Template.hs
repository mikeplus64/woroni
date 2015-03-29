{-# LANGUAGE NamedFieldPuns, NoMonoLocalBinds, OverloadedStrings, QuasiQuotes,
             RankNTypes, RecordWildCards #-}
module Template where
import Heist
import Heist.Compiled
import Heist.Compiled.LowLevel
import Snap
import Snap.Snaplet.PostgresqlSimple (liftPG)

import Control.Lens
import Data.Foldable
import Data.IORef
import Data.List
import Data.Monoid

import qualified Data.Text     as T
import           Data.Time
import           System.Locale

import DB
import Woroni

templates :: SpliceConfig H
templates = mempty
  & scCompiledSplices  .~ splices

spliceFrom :: (a -> T.Text) -> Getting (First a) View a -> Splice H
spliceFrom toText f = pureSplice
  (textSplice $ \t -> case t of
    Just t' -> toText t'
    Nothing -> "spliceFrom: invalid template")
  (preview f <$> lift (view page))

--------------------------------------------------------------------------------
-- Comment splices

commentBy :: ((Comment -> T.Text) -> v) -> Splices v
commentBy f = do
  "comment-id"      ## f (formatId . commentId)
  "comment-content" ## f commentContent
  "comment-author"  ## f (formatAuthor . commentAuthor)
  "comment-times"   ## f (formatTimes . commentTimes)

summaries :: Splice H
summaries =
  manyWithSplices
    (callTemplate "summary")
    (do let spliceSummary f = pureSplice (textSplice (f . summaryPost))
        "summary"         ## \summaryR -> do
          contents    <- runChildren
          isSelectedA <- pureSplice (textSplice formatSummaryTag) summaryR
          return (isSelectedA <> contents <> yieldPureText "</a>")

        "summary-date"    ## spliceSummary (formatTimes . postTimes)
        "summary-id"      ## spliceSummary (formatId . postId)
        "summary-content" ## spliceSummary postContent
        "summary-title"   ## spliceSummary postTitle
        "summary-times"   ## spliceSummary (formatTimes . postTimes)
        "summary-authors" ## spliceSummary (formatAuthors . postAuthors)
        "summary-tags"    ## spliceSummary (T.concat . map formatTag . postTags))
    (getSummariesFromView)

getSummariesFromView :: RuntimeSplice H [Summary]
getSummariesFromView = do
  p <- lift (view page)
  case p of
    -- if we're on a page, we want to get summaries of articles
    -- that are "around" that article
    PostView post   -> lift $ liftPG $ \pg ->
      getSummariesAround pg (postId post)
    -- otherwise, just get the most recent ones
    Home        -> undefined
    -- or, oh boy, this is exciting ...
    Search q    -> undefined
    _           -> return []

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
          "post-content"  ## splicePost postContent
          "post-title"    ## splicePost postTitle
          "post-times"    ## splicePost (formatTimes . postTimes)
          "post-authors"  ## splicePost (formatAuthors . postAuthors)
          "post-tags"     ## splicePost (formatTags . postTags)
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

  ------------------------------------------------------------------------------
  -- Misc
  "all-tags" ## deferMany
    (pureSplice (textSplice formatTag))
    (liftIO . readIORef =<< lift (view allTags))

--------------------------------------------------------------------------------
-- Formatters

formatId :: Id a -> T.Text
formatId = T.pack . show . fromId

formatTimes :: Times -> T.Text
formatTimes (Times c _) = T.pack (formatTime defaultTimeLocale "%B %d, %Y" c)

formatAuthor :: Author -> T.Text
formatAuthor Author{..} = authorName

formatAuthors :: [Author] -> T.Text
formatAuthors = oxfordCommas formatAuthor

formatTags :: [Tag] -> T.Text
formatTags = oxfordCommas formatTag

formatSummaryTag :: Summary -> T.Text
formatSummaryTag (Summary s p) =
  if s
  then "<a href='/post/" <> formatId (postId p) <> "' class='summary-selected'>"
  else "<a href='/post/" <> formatId (postId p) <> "' class='summary'>"

formatTag :: Tag -> T.Text
formatTag tag =
  "<a class='tag' href='/tag/"
  <> (formatId . tagId) tag
  <> "'>"
  <> tagName tag
  <> "</a>"

oxfordCommas :: (a -> T.Text) -> [a] -> T.Text
oxfordCommas _   []     = "none"
oxfordCommas fmt [x,y]  = fmt x <> " and " <> fmt y
oxfordCommas fmt (x:xs) =
  case xs of
    [] -> fmt x
    ys -> oxfordCommas1 (fmt x) ys
 where
  oxfordCommas1 acc [x]    = acc `seq` acc <> ", and " <> fmt x
  oxfordCommas1 acc (x:xs) = acc `seq` oxfordCommas1 (acc <> ", " <> fmt x) xs
  oxfordCommas1 _   _      = error "impossible"

--------------------------------------------------------------------------------
-- Util

pureText :: Monad n => (a -> T.Text) -> RuntimeSplice n a -> Splice n
pureText = pureSplice . textSplice
