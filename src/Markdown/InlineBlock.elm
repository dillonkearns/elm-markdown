module Markdown.InlineBlock exposing (Style, LinkUrl(..), StyledString)

{-| Represents styled inline text. For example, a header can include links, emphasis, etc.
See `Markdown.Block`.

@docs Style, LinkUrl, StyledString

-}


{-| -}
type alias Style =
    { isCode : Bool
    , isBold : Bool
    , isItalic : Bool
    , link : Maybe { title : Maybe String, destination : LinkUrl }
    }


{-| A link is either to an image URL or a page URL.
-}
type LinkUrl
    = Image String
    | Link String


{-| A string and its accompnaying styles.
-}
type alias StyledString =
    { style : Style, string : String }
