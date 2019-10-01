module Markdown.HtmlRenderer exposing (Attribute, HtmlRenderer(..))

import Markdown.Block exposing (Block)


type alias Attribute =
    { name : String, value : String }


type HtmlRenderer a
    = HtmlRenderer (String -> List Attribute -> List Block -> Result String a)
