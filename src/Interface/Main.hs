{-# LANGUAGE OverloadedStrings #-}
module Interface.Main where
import qualified Data.Text    as T
import qualified Data.Text.IO as T

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

main :: IO ()
main = do
  opts <- getArgs

  case opts of
    "new":"post":"in":stags:"by":said:[] -> do
      let tags = map Id (read stags)
          aids = map Id (read said)

      db      <- initDB
      title   <- T.getLine
      content <- pandoc =<< T.getContents
      pid     <- addPost db tags aids title content
      print pid

    "new":"author":name:email:[] -> do
      db  <- initDB
      aid <- addAuthor db (T.pack name) (Inet "127.0.0.1") (Just (T.pack email))
      print aid

    _ -> return ()

