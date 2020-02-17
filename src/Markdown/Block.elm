module Markdown.Block exposing
    ( Block(..)
    , Inline(..)
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
    = Heading Int (List Inline)
    | Body (List Inline)
    | Html String (List Attribute) (List Block)
    | UnorderedListBlock
        (List
            { task : Maybe Bool
            , body : List Inline
            }
        )
    | OrderedListBlock Int (List (List Inline))
    | CodeBlock Markdown.CodeBlock.CodeBlock
    | ThematicBreak
    | BlockQuote (List Block)


{-| Represents styled inline text. For example, a header can include links, emphasis, etc.
-}
type Inline
    = Link { href : String } (List Inline)
    | Bold Inline
    | Italic Inline
    | Image { src : String, alt : String }
    | Text String
    | CodeSpan String
    | HardLineBreak
