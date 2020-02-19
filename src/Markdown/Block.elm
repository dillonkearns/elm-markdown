module Markdown.Block exposing
    ( Block(..)
    , Inline
    )

{-|

@docs Block

-}

import Markdown.CodeBlock
import Markdown.Inline


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


## `HtmlComment`s

`HtmlComment`s contain the raw comment text, completely unprocessed. That means
you'll need to trim it if you want to strip the leading or trailing whitespace.

Renderer's do not process `HtmlComment`s. If you want to do any special processing
based on HTML comments, you can inspect the `Markdown.Block.Block`s before rendering
it and perform any special processing based on that. You could even add or remove
`Block`s, for example, based on the presence of certain comment values.

-}
type Block
    = Heading Int (List Inline)
    | Body (List Inline)
    | Html String (List Attribute) (List Block)
    | HtmlComment String
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


type alias Inline =
    Markdown.Inline.Inline (List Block)



--type Inline
--    = Link { href : String } (List Inline)
--    | Bold Inline
--    | Italic Inline
--    | Image { src : String, alt : String }
--    | Text String
--    | CodeSpan String
--    | HardLineBreak
