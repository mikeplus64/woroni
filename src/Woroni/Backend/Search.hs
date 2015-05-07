{-# LANGUAGE MultiWayIf, NoImplicitPrelude, OverloadedStrings, TemplateHaskell
             #-}
module Woroni.Backend.Search where
import           Control.Monad.State
import qualified Data.Text           as T
import           Woroni.Prelude

process :: Text -> Text
process a
  | T.null a  = ""
  | otherwise = procEach (T.words a)

procEach :: [Text] -> Text
procEach = T.intercalate "&" . fmap quote . filter (not . T.null) . fmap escape
 where
  escape  = T.filter (\a -> a /= '"' && a /= '\'') . T.strip
  quote a = "'" <> a <> "':*"
