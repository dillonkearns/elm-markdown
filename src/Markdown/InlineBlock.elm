module Markdown.InlineBlock exposing (LinkUrl(..), Style, StyledString)


type alias Style =
    { isCode : Bool
    , isBold : Bool
    , isItalic : Bool
    , link : Maybe { title : Maybe String, destination : LinkUrl }
    }


type LinkUrl
    = Image String
    | Link String


type alias StyledString =
    { style : Style, string : String }
