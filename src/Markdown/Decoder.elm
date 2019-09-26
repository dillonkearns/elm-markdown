module Markdown.Decoder exposing (..)

import Markdown.Block exposing (Block)


type alias Attribute =
    { name : String, value : String }


type Decoder a
    = Decoder (String -> List Attribute -> List Block -> Result String a)
