{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where
--------------------------------------------------------------------------------
import Snap
import Snap.Snaplet.Auth                           hiding (session)
import Snap.Snaplet.Hasql
import Snap.Snaplet.Heist.Compiled
import Snap.Snaplet.ReCaptcha
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Snaplet.Session.SessionManager
import Snap.Util.FileServe
import Snap.Util.Readable
--------------------------------------------------------------------------------
import Hasql.Postgres
--------------------------------------------------------------------------------
import Woroni.Application
import Woroni.Backend
import Woroni.Prelude
import Woroni.Routes
import Woroni.Template
--------------------------------------------------------------------------------

makeWoroni :: SnapletInit Woroni Woroni
makeWoroni = makeSnaplet "woroni" "The Woroni website" Nothing $ do
  _postgres <- nestSnaplet "" postgres $ hasqlInit
    (StringSettings "user=postgres host=localhost dbname=woroni")
    (fromJust (poolSettings 8 10))
  _heist    <- nestSnaplet "heist"    heist  $ heistInit ""
  _captcha  <- nestSnaplet "captcha" captcha $ initReCaptcha (Just _heist)
  addConfig _heist templates
  addRoutes routes
  return Woroni{ _page = Home, .. }

main :: IO ()
main = serveSnaplet mempty makeWoroni
  {-
  _postgres <- nestSnaplet "postgres" postgres pgsInit
  _heist    <- nestSnaplet "heist"    heist    (heistInit "")
  _captcha  <- nestSnaplet "captcha" captcha $
               initReCaptcha (Just _heist)
  _allTags  <- do
    tags <- liftIO $ getAllTags `runReaderT` _postgres
    liftIO (newIORef tags)
  addRoutes routes
  addConfig _heist templates
  return Woroni{_page = Home, ..}
-}
