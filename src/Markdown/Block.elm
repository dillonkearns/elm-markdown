module Markdown.Block exposing (Block(..))

{-|

@docs Block

-}

import Markdown.CodeBlock
import Markdown.InlineBlock as InlineBlock


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
    = Heading Int (List InlineBlock.StyledString)
    | Body (List InlineBlock.StyledString)
    | Html String (List Attribute) (List Block)
    | ListBlock (List (List InlineBlock.StyledString))
    | CodeBlock Markdown.CodeBlock.CodeBlock
    | ThematicBreak
    | BlankLine
