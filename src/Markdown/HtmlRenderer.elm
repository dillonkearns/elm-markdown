module Markdown.HtmlRenderer exposing (Attribute, HtmlRenderer(..), InlinesOrBlocks(..))

import Markdown.Block exposing (Block, Inline)


type alias Attribute =
    { name : String, value : String }


type InlinesOrBlocks
    = Inlines (List Inline)
    | Blocks (List Block)


type HtmlRenderer a
    = HtmlRenderer (String -> List Attribute -> InlinesOrBlocks -> Result String a)
