{-# LANGUAGE DataKinds, FlexibleInstances, OverloadedStrings, RecordWildCards,
             TemplateHaskell, TypeOperators #-}
module Woroni where

import Heist                (HeistT (..), RuntimeSplice, SpliceConfig,
                             scCompiledSplices)
import Heist.Compiled
import Heist.Internal.Types (RuntimeSplice (..))
import Heist.SpliceAPI

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
import Control.Monad.Reader
import Control.Monad.State.Strict as S

import Data.Monoid
import Data.String
import Data.Time
import Data.Time.Format
import System.Locale    (defaultTimeLocale)

import qualified Blaze.ByteString.Builder as Blaze
import qualified Data.Text                as T
import qualified Data.Vector              as V

import           Data.IORef
import           DB
import qualified DB.Schema  as Schema

data View
  = PostView    !Post
  | CommentView !Comment
  | Submit
  | Search !T.Text !(Aggregate Schema.Post)
  | Home
 deriving Show


type H = Handler Woroni Woroni

data Woroni = Woroni
  { _postgres :: Snaplet Postgres
  , _heist    :: Snaplet (Heist Woroni)
  , _captcha  :: Snaplet ReCaptcha

    -- | the current browser's "view"
  , _page     :: !View

  , _allTags  :: !(IORef [Tag])
  }


makePrisms ''View
makeLenses ''Woroni

instance HasHeist Woroni where heistLens = subSnaplet heist

instance HasPostgres (Handler b Woroni) where
  getPostgresState        = view (postgres.snapletValue)
  setLocalPostgresState a = local (postgres.snapletValue .~ a)

instance HasReCaptcha Woroni where captchaLens = subSnaplet captcha

