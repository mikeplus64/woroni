{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where
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

import Data.IORef
import Data.Monoid

import Control.Lens
import Control.Monad.Reader

import Routes
import Schema
import Template
import Woroni

main :: IO ()
main = serveSnaplet mempty $ makeSnaplet "woroni" "" Nothing $ do
  _postgres <- nestSnaplet "postgres" postgres pgsInit
  _heist    <- nestSnaplet "heist"    heist    (heistInit "")
  _captcha  <- nestSnaplet "captcha" captcha $
               initReCaptcha' (Just _heist) ("asdf", "fdsa")

  _allTags  <- liftIO $ do
    tags <- liftPG getAllTags `runReaderT` _postgres
    newIORef tags

  addRoutes routes

  addConfig _heist templates

  return Woroni{_page = Home, ..}
