module Markdown.Block exposing
    ( Block(..)
    , HeadingLevel(..), headingLevelToInt
    , ListItem(..), Task(..)
    , Html(..)
    , Inline(..)
    , HtmlAttribute
    , extractInlineText
    )

{-|

@docs Block
@docs HeadingLevel, headingLevelToInt


### List Items

@docs ListItem, Task


## HTML

@docs Html

See <Markdown.Html> for more.


## Inlines

@docs Inline
@docs HtmlAttribute
@docs extractInlineText

-}

import Markdown.CodeBlock


{-| This is the AST (abstract syntax tree) that represents your parsed markdown.

In the simplest case, you can pass this directly to a renderer:

    module Main exposing (main)

    import Markdown.Block exposing (Block)
    import Markdown.Parser
    import Markdown.Renderer

    markdown : String
    markdown =
        "# This is a title!\n\nThis is the body."

    astResult : Result (List (Advanced.DeadEnd String Parser.Problem)) (List Block)
    astResult =
        markdown
            |> Markdown.Parser.parse

    main : Html msg
    main =
        case
            astResult
                |> Result.mapError deadEndsToString
                |> Result.andThen (\ast -> Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer ast)
        of
            Ok rendered ->
                div [] rendered

            Err errors ->
                text errors

-}
type Block
    = -- Container Blocks
      HtmlBlock (Html Block)
    | UnorderedList (List (ListItem Inline))
    | OrderedList Int (List (List Inline))
    | BlockQuote (List Block)
      -- Leaf Blocks With Inlines
    | Heading HeadingLevel (List Inline)
    | Paragraph (List Inline)
      -- Table TODO  https://github.github.com/gfm/#tables-extension-
      -- Leaf Blocks Without Inlines
    | CodeBlock { body : String, language : Maybe String }
    | ThematicBreak


{-| The value for an unordered list item, which may contain a task.
-}
type ListItem children
    = ListItem Task (List children)


{-| A task (or no task), which may be contained in a ListItem.
-}
type Task
    = NoTask
    | IncompleteTask
    | CompletedTask


{-| Markdown limits headings to level 6 or less. HTML does this, too (`<h7>` is not supported by most browsers).
This type represents the possible heading levels that a Markdown heading block may have.

If you do a heading level above 6, the `#` characters will be treated as literal `#`'s.

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
    = HtmlInline (Html Block)
    | Link String (Maybe String) (List Inline)
    | Image String (Maybe String) (List Inline)
    | Emphasis (List Inline)
    | Strong (List Inline)
      -- Strikethrough TODO  https://github.github.com/gfm/#strikethrough-extension-
    | CodeSpan String
    | Text String
    | HardLineBreak


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
extractInlineText : List Inline -> String
extractInlineText inlines =
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
            text ++ extractInlineText inlines

        Image _ _ inlines ->
            text ++ extractInlineText inlines

        HtmlInline html ->
            --case html of
            --    HtmlElement _ _ inlines ->
            --text ++ extractText inlines
            --_ ->
            text

        Strong inlines ->
            text ++ extractInlineText inlines

        Emphasis inlines ->
            text ++ extractInlineText inlines


{-|


## `HtmlComment`s

`HtmlComment`s contain the raw comment text, completely unprocessed. That means
you'll need to trim it if you want to strip the leading or trailing whitespace.

Renderer's do not process `HtmlComment`s. If you want to do any special processing
based on HTML comments, you can inspect the `Markdown.Block.Block`s before rendering
it and perform any special processing based on that. You could even add or remove
`Block`s, for example, based on the presence of certain comment values.

-}
type Html children
    = HtmlElement String (List HtmlAttribute) (List children)
    | HtmlComment String
    | ProcessingInstruction String
    | HtmlDeclaration String String
    | Cdata String


{-| TODO
-}
type alias HtmlAttribute =
    { name : String, value : String }
