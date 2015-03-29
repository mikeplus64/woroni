{-# LANGUAGE OverloadedStrings #-}
module Routes where
import Data.ByteString (ByteString)

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

import Control.Lens

import Types
import Woroni

getId s = fmap Id $ fromBS =<< failIf "No id" (getParam s)

routes :: [(ByteString, H ())]
routes =
  [ ("/post/:id", do
        pid  <- getId "id"
        post <- failIf "Doesn't exist" . liftPG $ \db -> getPost db pid
        page .= Post post
        render "post")

  , ("/comment/:id/", do
        cid     <- getId "id"
        comment <- failIf "Doesn't exist" . liftPG $ \db -> getComment db cid
        page .= Comment comment
        render "comment")

  , ("/thread/:id", do
        pid  <- getId "id"
        post <- failIf "Doesn't exist" . liftPG $ \db -> getPost db pid
        page .= Post post
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
