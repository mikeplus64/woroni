{-# LANGUAGE OverloadedStrings #-}
module Routes where
import Data.ByteString    (ByteString)
import Data.Text.Encoding (decodeUtf8)

import Snap
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.PostgresqlSimple
import Snap.Snaplet.Heist.Compiled
import Snap.Snaplet.PostgresqlSimple
import Snap.Snaplet.ReCaptcha
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Snaplet.Session.SessionManager
import Snap.Util.FileServe
import Snap.Util.Readable

import Control.Applicative
import Control.Lens

import DB
import Woroni

getId s = fmap Id $ fromBS =<< failIf "No id" (getParam s)

setPostFromId = do
  pid  <- getId "id"
  post <- failIf "Doesn't exist" . liftPG $ \db -> getPost db pid
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
          liftPG $ \db -> addComment db author pid content
          here <- getsRequest rqPathInfo
          redirect here))
    )

  , ("/comment/:id/", do
        cid     <- getId "id"
        comment <- failIf "Doesn't exist" . liftPG $ \db -> getComment db cid
        page .= CommentView comment
        render "comment")

  , ("/thread/:id", do
        setPostFromId
        render "thread")

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
