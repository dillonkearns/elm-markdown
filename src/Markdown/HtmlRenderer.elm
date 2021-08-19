module Markdown.HtmlRenderer exposing (Attribute, HtmlRenderer(..), InlineOrBlock(..))

import Markdown.Block exposing (Block, Inline)


type alias Attribute =
    { name : String, value : String }


type InlineOrBlock
    = Inline Inline
    | Block Block


type HtmlRenderer a
    = HtmlRenderer (String -> List Attribute -> List InlineOrBlock -> Result String a)
