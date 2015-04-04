{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Monad.Reader
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Database.PostgreSQL.Simple
import           Snap.Snaplet.PostgresqlSimple

import qualified Data.HashMap.Strict as H
import qualified Data.Vector         as V
import qualified Data.Yaml           as Yaml

import DB.Query
import DB.Schema

import           System.Environment
import qualified System.IO          as IO
import           System.Process

pandoc :: (IO.Handle -> IO ()) -> IO T.Text
pandoc hPutStr = do
  (Just stdin, Just stdout, _, p) <- createProcess (proc "pandoc" opts)
    { std_in = CreatePipe
    , std_out = CreatePipe
    }
  hPutStr stdin
  IO.hFlush stdin
  IO.hClose stdin
  out <- T.hGetContents stdout
  terminateProcess p
  return out
 where
  opts = [ "--normalize"
         , "--html-q-tags"
         ]

yaml :: B.ByteString -> Maybe (H.HashMap T.Text Yaml.Value)
yaml s = case B.breakSubstring "---" s of
  ("", r) -> case B.breakSubstring "---" (B.drop 3 r) of
    (yams, s) | "---" `B.isPrefixOf` s -> Yaml.decode yams
    _ -> error "nope"
  _ -> error "nope"

initDB :: IO Connection
initDB = connect defaultConnectInfo{connectUser = "mike"
                                   ,connectDatabase="woroni"
                                   ,connectPassword = "fuggen secure :DDD"}
ids :: V.Vector Yaml.Value -> [Id a]
ids = V.foldr (\n xs -> case Yaml.decode (Yaml.encode n) of
                  Just a  -> Id a : xs
                  Nothing -> xs) []

main :: IO ()
main = do
  db   <- initDB
  let
    withDB :: ReaderT Postgres IO a -> IO a
    withDB f = runReaderT f (PostgresConn db)

  opts <- getArgs
  case opts of
    "new":"tag":name:[] -> do
      print =<< withDB (addTag (T.pack name))

    "new":"post":[] -> do

      raw     <- B.getContents
      let meta = yaml raw
      case meta of
        Just m | Just (Yaml.Array  tags')   <- H.lookup "tags"    m
               , Just (Yaml.Array  authors) <- H.lookup "authors" m
               , image                      <- H.lookup "image"   m
               , Just (Yaml.String title)   <- H.lookup "title"   m
               -> do
          print m
          tags <- V.toList `fmap` V.mapM (withDB . addTag . fromYStr) tags'
          html <- pandoc (`B.hPutStr` raw)
          img  <- case image of
            Just (Yaml.String i) -> Just `fmap` pandoc (`T.hPutStr` i)
            Nothing              -> return Nothing
          pid  <- withDB $ addPost
            tags
            (ids authors)
            img
            title
            html
          print pid
        Nothing -> error "No tags"

    "new":"author":name:email:[] -> do
      aid <- withDB $ addAuthor
             (T.pack name)
             (Inet "127.0.0.1")
             (Just (T.pack email))

      print aid

    _ -> return ()

fromYStr (Yaml.String a) = a
