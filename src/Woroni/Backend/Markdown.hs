{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
-- | Render markdown from Text, to Text
module Woroni.Backend.Markdown where
--------------------------------------------------------------------------------
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy     as L
--------------------------------------------------------------------------------
import qualified Blaze.ByteString.Builder      as Blaze
import qualified Text.Blaze.Html.Renderer.Text as Blaze
--------------------------------------------------------------------------------
import           Text.XmlHtml (Node (..))
import qualified Text.XmlHtml as XmlHtml
--------------------------------------------------------------------------------
import qualified Text.Markdown as MD
--------------------------------------------------------------------------------
import qualified Data.List as List
--------------------------------------------------------------------------------
import Woroni.Prelude

trustedSettings :: MD.MarkdownSettings
trustedSettings = MD.def
  { MD.msXssProtect            = False
  , MD.msFencedHandlers        = MD.codeFencedHandler "```"
  , MD.msBlockCodeRenderer     = \_lang (_src, html) -> html
  , MD.msLinkNewTab            = True
  , MD.msBlankBeforeBlockquote = False
  , MD.msBlockFilter           = id
  , MD.msAddHeadingId          = True
  }

untrustedSettings :: MD.MarkdownSettings
untrustedSettings = MD.def
  { MD.msXssProtect            = True
  , MD.msFencedHandlers        = MD.codeFencedHandler "```"
  , MD.msBlockCodeRenderer     = \_lang (_src, html) -> html
  , MD.msLinkNewTab            = True
  , MD.msBlankBeforeBlockquote = False
  , MD.msBlockFilter           = id
  , MD.msAddHeadingId          = False
  }

renderMarkdownTrusted :: Text -> Text
renderMarkdownTrusted =
  either (error "renderMarkdown")
         (T.decodeUtf8
          . Blaze.toByteString
          . XmlHtml.renderHtmlFragment XmlHtml.UTF8
          . img2figure
          . XmlHtml.docContent)

  . XmlHtml.parseHTML "markdown"
  . T.encodeUtf8
  . L.toStrict
  . Blaze.renderHtml
  . MD.markdown trustedSettings
  . L.fromStrict

img2figure :: [Node] -> [Node]
img2figure = map $ \node ->
  case node of
    Element{..}
      | elementTag == "img",
        Just caption <- lookup "alt"   elementAttrs,
        Just credit  <- lookup "title" elementAttrs ->
        Element "figure" []
        [ node{ elementAttrs =
                List.filter (attrIsn't ["alt","title"]) elementAttrs
              }
        , Element "figcaption" []                   [TextNode caption]
        , Element "figcaption" [("class","credit")] [TextNode credit]
        ]

      | elementTag == "img"
      , Just caption <- lookup "alt" elementAttrs
      , caption /= "" ->

        Element "figure" []
        [ node{ elementAttrs =
                List.filter (attrIsn't ["alt"]) elementAttrs
              }
        , Element "figcaption" [] [TextNode caption]
        ]
      | otherwise -> node{elementChildren = img2figure elementChildren}
    _ -> node

attrIsn't :: [Text] -> (Text,Text) -> Bool
attrIsn't attrs (attr, _) = attr `List.notElem` attrs

renderMarkdown :: Text -> Text
renderMarkdown =
  L.toStrict
  . Blaze.renderHtml
  . MD.markdown untrustedSettings
  . L.fromStrict


