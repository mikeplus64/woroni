{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Monad.Reader
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Database.PostgreSQL.Simple
import           Snap.Snaplet.PostgresqlSimple

import DB.Query
import DB.Schema

import System.Environment
import System.IO
import System.Process

pandoc :: T.Text -> IO T.Text
pandoc i = do
  (Just stdin, Just stdout, _, p) <- createProcess (proc "pandoc" opts)
    { std_in = CreatePipe
    , std_out = CreatePipe
    }
  T.hPutStr stdin i
  hFlush stdin
  hClose stdin
  out <- T.hGetContents stdout
  terminateProcess p
  return out
 where
  opts = [ "--normalize"
         , "--html-q-tags"
         ]

initDB :: IO Connection
initDB = connect defaultConnectInfo{connectUser = "mike"
                                   ,connectDatabase="woroni"
                                   ,connectPass = "fuggen secure :DDD"}

main :: IO ()
main = do
  db   <- initDB
  let withDB f = runReaderT f (PostgresConn db)
  opts <- getArgs
  case opts of
    "new":"post":"in":stags:"by":said:[] -> do
      let tags = map Id (read stags)
          aids = map Id (read said)


      title   <- T.getLine
      content <- pandoc =<< T.getContents
      pid     <- withDB $ addPost tags aids title content
      print pid

    "new":"author":name:email:[] -> do
      aid <- withDB $ addAuthor (T.pack name) (Inet "127.0.0.1") (Just (T.pack email))
      print aid

    _ -> return ()

