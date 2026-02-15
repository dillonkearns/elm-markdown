module Markdown.HtmlRenderer exposing (Attribute, HtmlRenderer(..))


type alias Attribute =
    { name : String, value : String }


type HtmlRenderer err a
    = HtmlRenderer (String -> List Attribute -> String -> Result err a)
