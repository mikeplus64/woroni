{-# LANGUAGE DataKinds, NoImplicitPrelude, OverloadedStrings #-}
module Woroni.Routes where
--------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding    as T
--------------------------------------------------------------------------------
import Snap
import Snap.Snaplet.Hasql
import Snap.Snaplet.Heist.Compiled
import Snap.Snaplet.ReCaptcha
import Snap.Util.FileServe
import Snap.Util.Readable
--------------------------------------------------------------------------------
import           Woroni.Application
import           Woroni.Backend
import qualified Woroni.Backend.Search as Search
import           Woroni.Prelude
--------------------------------------------------------------------------------

idInt :: Int -> Id'
idInt = (fromIntegral :: Int -> Int32)

id' :: ByteString -> Maybe Id'
id' = fmap idInt . fromBS

getInt32 :: MonadSnap m => ByteString -> m (Id')
getInt32 s = fmap idInt . fromBS =<< failIf "No int32" (getParam s)

getInt32s :: MonadSnap m => ByteString -> m [Id']
getInt32s p =
  case B.split ',' p of
    [] -> fail "No id."
    xs -> case mapM (fmap idInt . fromBS) xs of
      Just ids -> return ids
      Nothing  -> fail "Cannot parse int32s."

setPostFromId :: H ()
setPostFromId = do
  pid      <- getInt32 "id"
  (art,ss) <- session $ tx Nothing $ do
    (,) <$> getArticle (Id pid)
        <*> pageAt (JustId (Id pid) :: MaybeId (Just Post)) 0 10
  case art of
    Just a  -> page .= PostView a ss
    Nothing -> fail "Invalid article."

search :: ByteString -> Int32 -> H ()
search templ pageN = do
  terms <- T.decodeUtf8 . maybe "" id <$> getQueryParam "terms"
  summs <- session $ tx Nothing $ postsSearch (Search.process terms) (pageN*15) 15
  page .= Search terms Nothing (Page (pageN*1) summs)
  render templ

tags :: H ()
tags = do
  pids  <- failIf "No id" (getParam "id")
  pages <- getInt32s pids

  pageN <- failIf "Couldn't parse page number" $
           maybe (Just 0) id' <$> getQueryParam "page"

  post  <- (id' =<<) <$> getQueryParam "post"

  summs <- session $ tx Nothing $
           tagSearch (maybe (Id maxBound) Id post) pages (pageN*15) 15

  page .= Search (T.decodeUtf8 pids) post (Page (pageN*15) summs)
  case post of
    Just _  -> render "aside-search"
    Nothing -> render "search"

comment :: Int32 -> H ()
comment pid = do
  checkCaptcha <|> fail "Invalid captcha response."
  mname        <- fmap T.decodeUtf8 <$> getPostParam "name"
  Just content <- fmap T.decodeUtf8 <$> getPostParam "content"
  memail       <- fmap T.decodeUtf8 <$> getPostParam "email"
  address <- getsRequest rqRemoteAddr
  now     <- liftIO getCurrentTime
  session $ tx txMode $
    newComment (Id pid) mname memail content address now
  return ()

routes :: [(ByteString, H ())]
routes =
  [ ("/post/:id"     , method GET $ setPostFromId >> render "post")

  , ("/search"       , method GET (search "search" 0))
  , ("/search/:page" , method GET (search "search" =<< getInt32 "page"))

  , ("/quicksearch"       , method GET (search "raw-search" 0))
  , ("/quicksearch/:page" , method GET (search "raw-search" =<< getInt32 "page"))

  , ("/tag/:id"      , method GET tags)
  , ("/tag/"         , method GET (return ()))

  , ("/author/:id"   , method GET tags)
  , ("/author/"      , method GET (return ()))

  , ("/post/:id/", method POST $ do
        pid <- getInt32 "id"
        comment pid
        setPostFromId
        render "post")

  , ("/feed.xml", method GET $ do
        summs <- session $ tx Nothing $ pageAt NoId 0 15
        page .= Search "Home" Nothing (Page 0 summs)
        writeText "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
        renderAs "application/rss+xml" "feed"
    )

  , ("/", method GET $ do
        summs <- session $ tx Nothing $ pageAt NoId 0 15
        page .= Search "Home" Nothing (Page 0 summs)
        render "home"
        )

{-
  , ("/thread/:id", do
        setPostFromId
        render "thread")

  , ("/tag/:id", do
        tids <- getInt32s "id"
        json <- getQueryParam "aside"
        case json of
          Just pid' -> do
            pid <- fromBS pid'
            page .= Search "tag search" (tagsAgg (Just tids)){ aggTop = Just (Id pid) }
            render "search"
          Nothing  -> do
            tags <- getTagNames tids
            page .= Search (oxfordCommas id tags) (tagsAgg (Just tids))
            render "tag-search"
    )

  , ("/tag/", do
        page .= Search "All" (tagsAgg Nothing))

  , ("/author/:id", do
        aids  <- getInt32s "id"
        names <- getAuthorNames aids
        page .= Search (oxfordCommas id names) (authorsAgg (Just aids))
        render "tag-search")

  , ("/", render "home")
  -}

  , ("/static", serveDirectory "static")
  ]

--------------------------------------------------------------------------------
-- Util

failIf :: Monad m => String -> m (Maybe a) -> m a
failIf s m = do
  m' <- m
  case m' of
   Just a  -> return a
   Nothing -> fail s

