{-# LANGUAGE OverloadedStrings, RecordWildCards, TemplateHaskell #-}
module Woroni.Interface.Main where
--------------------------------------------------------------------------------
import Snap
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.JsonFile
import Snap.Snaplet.Hasql
import Snap.Snaplet.Heist.Compiled
import Snap.Snaplet.Session
import Snap.Snaplet.Session
import Snap.Snaplet.Session.Backends.CookieSession
--------------------------------------------------------------------------------
import Hasql.Postgres
--------------------------------------------------------------------------------
import Woroni.Backend
import Woroni.Prelude
--------------------------------------------------------------------------------
import Data.String
import System.Random

data I = I
  { _heist    :: !(Snaplet (Heist I))
  , _postgres :: !(Snaplet (Pool Postgres))
  , _authmgr  :: !(Snaplet (AuthManager I))
  , _sessmgr  :: !(Snaplet SessionManager)
  , _stdGen   :: !StdGen
  }

makeLenses ''I

instance HasHeist I where heistLens = subSnaplet heist
instance HasPool I Postgres where poolLens = postgres.snapletValue

random128 :: StdGen -> (String, StdGen)
random128 g0 = go "" g0 0
 where
  go str g n
    | n < 128 = case randomR ('a', 'z') g of
                 (c, g') -> go (c:str) g' (n+1)
    | otherwise = (str, g)

routes =
  [("/new_admin", do
       gen <- use stdGen
       let (password, gen') = random128 gen
           bstr             = fromString password
       exists <- with authmgr (usernameExists "admin")
       if not exists
         then do
           admin <- with authmgr (createUser "admin" bstr)
           liftIO (putStrLn password)
           writeText "OK! A brand new admin"
           writeText (fromString (show admin))
         else do
           writeText "it exists?"
   )]

main :: IO ()
main =
  serveSnaplet defaultConfig $
  makeSnaplet "woroni admin interface" "woroni admin" Nothing $ do
    _postgres <- nestSnaplet "" postgres $ hasqlInit
      (StringSettings "user=postgres host=localhost dbname=woroni")
      (fromJust (poolSettings 8 10))
    _heist    <- nestSnaplet "heist"    heist  $ heistInit "interface"
    _sessmgr  <- nestSnaplet "session" sessmgr $
                 initCookieSessionManager "interface_site_key.txt" "woroni" (Just 240)
    _authmgr  <- nestSnaplet "auth" authmgr $
                 initJsonFileAuthManager defAuthSettings sessmgr "woroni.json"
    _stdGen   <- liftIO newStdGen
    addRoutes routes
    return I{..}


