{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses,
             OverloadedStrings, RecordWildCards, TemplateHaskell, TypeOperators
             #-}
module Woroni.Application where
--------------------------------------------------------------------------------
import Snap
import Snap.Snaplet.Hasql
import Snap.Snaplet.Heist.Compiled
import Snap.Snaplet.ReCaptcha
--------------------------------------------------------------------------------
import Hasql.Postgres
--------------------------------------------------------------------------------
import Woroni.Backend
import Woroni.Prelude
--------------------------------------------------------------------------------

data Woroni = Woroni
  { _postgres :: !(Snaplet (Pool Postgres))
  , _heist    :: !(Snaplet (Heist Woroni))
  , _captcha  :: !(Snaplet ReCaptcha)
  , _page     :: !View
  }

data View
  = PostView !Article !(Vector Summary)
  | Thread !(Page (Just Post) Comment)
  | AsidePosts !(Page (Just Post) Summary)
  | Search !Text !(Page Nothing Summary)
  | Home

type H = Handler Woroni Woroni

makePrisms ''View
makeLenses ''Woroni

instance HasHeist Woroni         where heistLens   = subSnaplet heist
instance HasPool Woroni Postgres where poolLens    = postgres.snapletValue
instance HasReCaptcha Woroni     where captchaLens = subSnaplet captcha
