module Markdown.Block exposing
    ( Block(..)
    , HeadingLevel(..)
    , Html(..)
    , Inline(..)
    , HtmlAttribute
    , extractText
    , headingLevelToInt
    )

{-|

@docs Block
@docs HeadingLevel


## HTML

See <Markdown.Html> for more.

@docs Html


## Inlines

@docs Inline
@docs HtmlAttribute
@docs extractText


## Convenience functions

@docs headingLevelToInt

-}

import Markdown.CodeBlock


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
    = Heading HeadingLevel (List Inline)
    | Paragraph (List Inline)
    | HtmlBlock Html
    | UnorderedListBlock
        -- TODO use the same type as the Renderer here
        -- in general, try to share types with Renderer as much as possible
        (List
            { task : Maybe Bool
            , body : List Inline
            }
        )
    | OrderedListBlock Int (List (List Inline))
    | CodeBlock Markdown.CodeBlock.CodeBlock
    | ThematicBreak
    | BlockQuote (List Block)


{-| TODO
-}
type HeadingLevel
    = H1
    | H2
    | H3
    | H4
    | H5
    | H6


{-| TODO an inline
-}
type Inline
    = Text String
    | HardLineBreak
    | CodeSpan String
    | Link String (Maybe String) (List Inline)
    | Image String (Maybe String) (List Inline)
    | HtmlInline Html
    | Emphasis (List Inline)
    | Strong (List Inline)


{-| TODO
-}
headingLevelToInt : HeadingLevel -> Int
headingLevelToInt headingLevel =
    case headingLevel of
        H1 ->
            1

        H2 ->
            2

        H3 ->
            3

        H4 ->
            4

        H5 ->
            5

        H6 ->
            6



--{-| TODO
---}
--extractText : List Inline -> String
--extractText inlines =
--    "TODO"
--


{-| Extract the text from a list of inlines.

    inlines : List (Inline)
    inlines =
        [ Text "Heading with "
        , Emphasis 1
            [ Text "emphasis" ]
        ]

    extractText inlines == "Heading with emphasis"

    -- Original string: "Heading with *emphasis*"

-}
extractText : List Inline -> String
extractText inlines =
    List.foldl extractTextHelp "" inlines


extractTextHelp : Inline -> String -> String
extractTextHelp inline text =
    case inline of
        Text str ->
            text ++ str

        HardLineBreak ->
            text ++ " "

        CodeSpan str ->
            text ++ str

        Link _ _ inlines ->
            text ++ extractText inlines

        Image _ _ inlines ->
            text ++ extractText inlines

        HtmlInline html ->
            case html of
                HtmlElement _ _ inlines ->
                    --text ++ extractText inlines
                    text

                _ ->
                    ""

        Strong inlines ->
            text ++ extractText inlines

        Emphasis inlines ->
            text ++ extractText inlines


{-| TODO
-}
type Html
    = HtmlComment String
    | HtmlElement String (List HtmlAttribute) (List Block)
    | ProcessingInstruction String
    | HtmlDeclaration String
    | Cdata String


{-| TODO
-}
type alias HtmlAttribute =
    { name : String, value : String }
