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
import Woroni.Application
import Woroni.Backend
import Woroni.Prelude
--------------------------------------------------------------------------------

idInt :: Int -> Id'
idInt = (fromIntegral :: Int -> Int32)

getInt32 :: MonadSnap m => ByteString -> m (Id')
getInt32 s = fmap idInt . fromBS =<< failIf "No int32" (getParam s)

getInt32s :: MonadSnap m => ByteString -> m [Id']
getInt32s s = do
  p <- failIf "No id" (getParam s)
  case B.split ',' p of
    [] -> fail "No id."
    xs -> case mapM (fmap idInt . fromBS) xs of
      Just ids -> return ids
      Nothing  -> fail "Cannot parse."

setPostFromId :: H ()
setPostFromId = do
  pid      <- getInt32 "id"
  (art,ss) <- session $ tx Nothing $ do
    (,) <$> getArticle (Id pid)
        <*> pageAt (JustId (Id pid) :: MaybeId (Just Post)) 0 10
  case art of
    Just a  -> page .= PostView a ss
    Nothing -> fail "Invalid article."

routes :: [(ByteString, H ())]
routes =
  [ ("/post/:id", method GET $ setPostFromId >> render "post")
  , ("/search/:page", method GET $ do
        terms <- T.decodeUtf8 <$> failIf "No terms" (getQueryParam "terms")
        pageN <- getInt32 "page"
        summs <- session $ tx Nothing $ postsSearch terms (pageN*15) 15
        page .= Search terms (Page (pageN*15) summs)
        render "search"
    )
                  {-
             <|>  method POST (withCaptcha
      (writeBS "Bad captcha, sorry!")
      (do pid  <- getInt32 "id"
          addr <- getsRequest rqRemoteAddr
          name <- getPostParam "name"
          Just content <- fmap decodeUtf8 <$> getPostParam "content"
          let author = createAuthor
                       (fmap decodeUtf8 (if name == Just ""
                                         then Nothing
                                         else name))
                       (Inet addr)
                       Nothing
          addComment author pid content
          here <- getsRequest rqPathInfo
          redirect here))
-}

{-
  , ("/comment/:id/", do
        cid     <- getInt32 "id"
        comment <- failIf "Doesn't exist" (getComment cid)
        page .= CommentView comment
        render "comment")

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

