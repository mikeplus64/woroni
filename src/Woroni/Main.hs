{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards #-}
module Main where
--------------------------------------------------------------------------------
import Snap
import Snap.Snaplet.Hasql
import Snap.Snaplet.Heist.Compiled
import Snap.Snaplet.ReCaptcha
--------------------------------------------------------------------------------
import Control.Monad.State
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

  v <- liftIO $! trydb (_postgres^.snapletValue) setSchema
  case v of
    Just _ -> return ()
    _      -> void . liftIO $ trydb (_postgres^.snapletValue) $
              unitEx [stmt|CREATE INDEX post_search_doc ON post_search
                           USING gin(document) |]

  tags' <- liftIO . fmap fromJust $! trydb (_postgres^.snapletValue) $
    vectorEx [stmt|SELECT * FROM tag|]

  authors' <- liftIO . fmap fromJust $! trydb (_postgres^.snapletValue) $
    vectorEx [stmt|SELECT * FROM author|]

  tagsRef    <- liftIO (newIORef tags')
  authorsRef <- liftIO (newIORef authors')

  addConfig _heist templates
  addRoutes routes
  return Woroni
    { _page       = Home
    , _allTags    = tagsRef
    , _allAuthors = authorsRef
    , ..
    }

updateAllTags :: Woroni -> IO ()
updateAllTags s = do
  Just tags' <- trydb (s^.postgres.snapletValue) $
                vectorEx [stmt|SELECT * FROM tag|]
  liftIO $! writeIORef (_allTags s) tags'

updateAllAuthors :: Woroni -> IO ()
updateAllAuthors s = do
  Just authors' <- trydb (s^.postgres.snapletValue) $
                vectorEx [stmt|SELECT * FROM tag|]
  liftIO $! writeIORef (_allAuthors s) authors'

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
