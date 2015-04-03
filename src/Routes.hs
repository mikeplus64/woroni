{-# LANGUAGE OverloadedStrings #-}
module Routes where
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Text.Encoding    (decodeUtf8)

import qualified Data.Text as T

import Snap
import Snap.Snaplet.Heist.Compiled
import Snap.Snaplet.PostgresqlSimple
import Snap.Snaplet.ReCaptcha
import Snap.Util.FileServe
import Snap.Util.Readable

import Data.Aeson (decode, encode)

import Control.Applicative
import Control.Lens
import Control.Monad.Trans

import DB
import Template
import Woroni

getId :: MonadSnap m => ByteString -> m (Id a)
getId s = fmap Id $ fromBS =<< failIf "No id" (getParam s)

getIds :: MonadSnap m => ByteString -> m [Id a]
getIds s = do
  p <- failIf "No id" (getParam s)
  case B.split ',' p of
    [] -> fail "No id."
    xs -> case mapM (fmap Id . fromBS) xs of
      Just ids -> return ids
      Nothing  -> fail "Cannot parse."

setPostFromId :: H ()
setPostFromId = do
  pid  <- getId "id"
  post <- failIf "Doesn't exist" (getPost pid)
  page .= PostView post

routes :: [(ByteString, H ())]
routes =
  [ ("/post/:id", method GET  (setPostFromId >> render "post")
             <|>  method POST (withCaptcha
      (writeBS "Bad captcha, sorry!")
      (do pid  <- getId "id"
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
    )

  , ("/comment/:id/", do
        cid     <- getId "id"
        comment <- failIf "Doesn't exist" (getComment cid)
        page .= CommentView comment
        render "comment")

  , ("/thread/:id", do
        setPostFromId
        render "thread")

  , ("/tag/:id", do
        tids <- getIds "id"
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
        aids  <- getIds "id"
        names <- getAuthorNames aids
        page .= Search (oxfordCommas id names) (authorsAgg (Just aids))
        render "tag-search")

  , ("/", render "home")

  , ("/static", serveDirectory "static")
  ]

search :: Aggregate a
search = Aggregate
  { aggLimit   = 10
  , aggSize    = 140
  , aggTop     = Nothing
  , aggTags    = Nothing
  , aggAuthors = Nothing
  , aggTerms   = Nothing
  }

tagsAgg :: Maybe [Id Tag] -> Aggregate a
tagsAgg tags = search{aggTags = tags}

authorsAgg :: Maybe [Id Author] -> Aggregate a
authorsAgg authors = search{aggAuthors = authors}

--------------------------------------------------------------------------------
-- Util

failIf :: Monad m => String -> m (Maybe a) -> m a
failIf s m = do
  m' <- m
  case m' of
   Just a  -> return a
   Nothing -> fail s
