module Markdown.Block exposing
    ( Block(..)
    , Inline(..), TopLevelInline(..)
    )

{-|

@docs Block


## Inlines

@docs Inline, TopLevelInline

-}

import Markdown.CodeBlock


type alias Attribute =
    { name : String, value : String }


{-| This is the AST (abstract syntax tree) that represents your parsed markdown.

In the simplest case, you can pass this directly to a renderer:

    module Main exposing (main)

    import Markdown.Block exposing (Block)
    import Markdown.Parser

    markdown : String
    markdown =
        "# This is a title!\n\nThis is the body."

    astResult : Result (List (Advanced.DeadEnd String Parser.Problem)) (List Block)
    astResult =
        markdown
            |> Markdown.Parser.parse

    main : Html msg
    main =
        case astResult of
            Ok ast ->
                Markdown.Parser.renderDefaultHtml ast

            Err errors ->
                Html.text "Encountered parsing errors."

-}
type Block
    = Heading Int (List TopLevelInline)
    | Body (List TopLevelInline)
    | Html String (List Attribute) (List Block)
    | UnorderedListBlock
        (List
            { task : Maybe Bool
            , body : List TopLevelInline
            }
        )
    | OrderedListBlock Int (List (List TopLevelInline))
    | CodeBlock Markdown.CodeBlock.CodeBlock
    | ThematicBreak
    | BlockQuote (List Block)


type TopLevelInline
    = Link { href : String } Inline
    | InlineContent Inline


{-| Represents styled inline text. For example, a header can include links, emphasis, etc.
-}
type Inline
    = Bold Inline
    | Italic Inline
    | Image String
    | Text String
    | CodeSpan String


example1 : TopLevelInline
example1 =
    Link { href = "" } <| Bold <| Italic <| Text "Hello"


example2 : TopLevelInline
example2 =
    Link { href = "" } <| Bold <| Italic <| Text "Hello"


example3 : TopLevelInline
example3 =
    InlineContent <| CodeSpan "this is code"
