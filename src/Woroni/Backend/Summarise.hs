{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- | Summarise HTML things
--------------------------------------------------------------------------------
module Woroni.Backend.Summarise where
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy     as TL
--------------------------------------------------------------------------------
import qualified Blaze.ByteString.Builder as Blaze
import           Text.XmlHtml             (Node (..))
import qualified Text.XmlHtml             as XmlHtml
--------------------------------------------------------------------------------
import Debug.Trace
import Woroni.Prelude
--------------------------------------------------------------------------------

summarise :: (a -> Text) -> a -> Text
summarise f t =
  case XmlHtml.parseHTML "summary" (T.encodeUtf8 (f t)) of
    Right html ->
      T.decodeUtf8
      . Blaze.toByteString
      . XmlHtml.renderHtmlFragment XmlHtml.UTF8
      . takeNodesTo summarySize
      $! XmlHtml.docContent html
    Left _     -> takeIsh summarySize (f t)

summarySize :: Int
summarySize = 200

summaryNode :: Int -> Node -> Node
summaryNode remSize n = case n of
  TextNode content -> TextNode (takeIsh remSize content)
  Element tag attrs childs ->
    Element tag attrs $! takeNodesTo remSize childs
  _ -> TextNode ""

takeNodesTo :: Int -> [Node] -> [Node]
takeNodesTo target = go 0
  where
    go !consumed (x:xs)
      | consumed < target =
          let summary = summaryNode (target-consumed) x
          in summary : go (consumed+nodeSize summary) xs
      | consumed > target = []
      | otherwise         = [TextNode "..."]
    go consumed _ = traceShow (consumed,target) []

    nodeSize (TextNode t)     = T.length t
    nodeSize (Element t _ cs)
      | t == "figcaption" = 0
      | otherwise         = foldl' (+) 0 (map nodeSize cs)
    nodeSize _ = 0

takeIsh :: Int -> Text -> Text
takeIsh n' str = go 0
 where
  n = min n' (T.length str)
  go !i
    | i < n                = go (i + 1)
    | i >= T.length str    = str
    | T.index str i == ' ' = T.take i str `T.append` "..."
    | otherwise            = go (i+1)
