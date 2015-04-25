-- | Render markdown from Text, to Text
module Woroni.Backend.Markdown where
--------------------------------------------------------------------------------
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import qualified Text.Sundown.Html.Text        as MD
--------------------------------------------------------------------------------
import Woroni.Prelude

renderMarkdown :: Text -> Text
renderMarkdown =
  MD.renderHtml
    MD.allExtensions
    MD.allHtmlModes
    False
    Nothing

renderMarkdownTrusted :: Text -> Text
renderMarkdownTrusted =
  MD.renderHtml
    MD.allExtensions
    MD.noHtmlModes
    False
    Nothing

