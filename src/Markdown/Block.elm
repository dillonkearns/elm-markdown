module Markdown.Block exposing
    ( Block(..)
    , HeadingLevel(..), headingLevelToInt
    , ListItem(..), Task(..)
    , Html(..)
    , Inline(..)
    , HtmlAttribute
    , extractInlineText
    , walkInlines, validateMapInlines, mapAccuml, foldl
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


## Transformations

@docs walkInlines, validateMapInlines, mapAccuml, foldl

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


{-| An Inline block. Note that `HtmlInline`s can contain Blocks, not just nested `Inline`s.
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


{-| A convenience function so that you don't have to write a big case statement if you need a heading level as an Int.
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


{-| The way HTML is handled is one of the core ideas of this library.

You get the full HTML structure that you can use to process the Blocks before rendering them. Once you render them,
all of the raw text within your HTML is parsed as Markdown.


## `HtmlComment`s and metadata

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


{-| An Html attribute. In <div class="foo">, you would have `{ name = "class", value = "foo" }`.
-}
type alias HtmlAttribute =
    { name : String, value : String }


{-| TODO
-}
validateMapInlines : (Inline -> Result error Inline) -> List Block -> Result (List error) (List Block)
validateMapInlines function blocks =
    let
        newThing : Block -> Result (List error) Block
        newThing block =
            case block of
                Paragraph inlines ->
                    inlines
                        |> List.map (inlineParserValidateWalk function)
                        |> combine
                        |> Result.map Paragraph

                _ ->
                    Debug.todo ""
    in
    blocks
        |> validateMap newThing


{-| TODO
-}
walkInlines : (Inline -> Inline) -> Block -> Block
walkInlines function block =
    walk (walkInlinesHelp function) block


walkInlinesHelp : (Inline -> Inline) -> Block -> Block
walkInlinesHelp function block =
    case block of
        Paragraph inlines ->
            List.map (inlineParserWalk function) inlines
                |> Paragraph

        UnorderedList listItems ->
            List.map
                (\(ListItem task children) ->
                    ListItem task (List.map (inlineParserWalk function) children)
                )
                listItems
                |> UnorderedList

        OrderedList startingIndex listItems ->
            List.map
                (List.map (inlineParserWalk function))
                listItems
                |> OrderedList startingIndex

        BlockQuote children ->
            BlockQuote (List.map (walkInlinesHelp function) children)

        Heading level children ->
            Heading level (List.map function children)

        HtmlBlock html ->
            case html of
                HtmlElement string htmlAttributes blocks ->
                    HtmlElement string
                        htmlAttributes
                        (List.map (walkInlinesHelp function) blocks)
                        |> HtmlBlock

                _ ->
                    block

        _ ->
            block


inlineParserWalk : (Inline -> Inline) -> Inline -> Inline
inlineParserWalk function inline =
    case inline of
        Link url maybeTitle inlines ->
            List.map (inlineParserWalk function) inlines
                |> Link url maybeTitle
                |> function

        Image url maybeTitle inlines ->
            List.map (inlineParserWalk function) inlines
                |> Image url maybeTitle
                |> function

        Emphasis inlines ->
            List.map (inlineParserWalk function) inlines
                |> Emphasis
                |> function

        --HtmlInline tag attrs inlines ->
        --    List.map (inlineParserWalk function) inlines
        --        |> HtmlInline tag attrs
        --        |> function
        _ ->
            function inline


inlineParserValidateWalk : (Inline -> Result error Inline) -> Inline -> Result (List error) Inline
inlineParserValidateWalk function inline =
    case inline of
        Link url maybeTitle inlines ->
            List.map (inlineParserValidateWalk function) inlines
                |> combine
                |> Result.andThen
                    (\nestedInlines ->
                        Link url maybeTitle nestedInlines
                            |> function
                            |> Result.mapError List.singleton
                    )

        --Image url maybeTitle inlines ->
        --    List.map (inlineParserWalk function) inlines
        --        |> Image url maybeTitle
        --        |> function
        --
        --Emphasis inlines ->
        --    List.map (inlineParserWalk function) inlines
        --        |> Emphasis
        --        |> function
        --HtmlInline tag attrs inlines ->
        --    List.map (inlineParserWalk function) inlines
        --        |> HtmlInline tag attrs
        --        |> function
        _ ->
            function inline
                |> Result.mapError List.singleton


walk : (Block -> Block) -> Block -> Block
walk function block =
    case block of
        BlockQuote blocks ->
            List.map (walk function) blocks
                |> BlockQuote
                |> function

        HtmlBlock html ->
            case html of
                HtmlElement string htmlAttributes blocks ->
                    HtmlElement string
                        htmlAttributes
                        (List.map (walk function) blocks)
                        |> HtmlBlock
                        |> function

                _ ->
                    function block

        _ ->
            function block


validateMap : (Block -> Result error value) -> List Block -> Result error (List value)
validateMap mapFn blocks =
    blocks
        |> List.map mapFn
        |> combine


{-| Combine a list of results into a single result (holding a list).
-}
combine : List (Result x a) -> Result x (List a)
combine =
    List.foldr (Result.map2 (::)) (Ok [])


{-| -}
mapAccuml : (soFar -> Block -> ( soFar, mappedValue )) -> soFar -> List Block -> ( soFar, List mappedValue )
mapAccuml function initialValue blocks =
    let
        ( accFinal, generatedList ) =
            foldl
                (\x ( acc1, ys ) ->
                    let
                        ( acc2, y ) =
                            function acc1 x
                    in
                    ( acc2, y :: ys )
                )
                ( initialValue, [] )
                blocks
    in
    ( accFinal, List.reverse generatedList )


{-| Fold over all blocks to yield a value.

    import Markdown.Block as Block exposing (..)

    maximumHeadingLevel : List Block -> Maybe HeadingLevel
    maximumHeadingLevel blocks =
        blocks
            |> Block.foldl
                (\block maxSoFar ->
                    case block of
                        Heading level _ ->
                            if Block.headingLevelToInt level > (maxSoFar |> Maybe.map Block.headingLevelToInt |> Maybe.withDefault 0) then
                                Just level
                            else
                                maxSoFar
                        _ ->
                            maxSoFar
                )
                Nothing

    [ Heading H1 [ Text "Document" ]
    , Heading H2 [ Text "Section A" ]
    , Heading H3 [ Text "Subsection" ]
    , Heading H2 [ Text "Section B" ]
    ]
        |> maximumHeadingLevel
    -->  (Just H3)

-}
foldl : (Block -> acc -> acc) -> acc -> List Block -> acc
foldl function acc list =
    case list of
        [] ->
            acc

        block :: remainingBlocks ->
            case block of
                HtmlBlock html ->
                    case html of
                        HtmlElement _ _ children ->
                            foldl function (function block acc) (children ++ remainingBlocks)

                        _ ->
                            foldl function (function block acc) remainingBlocks

                UnorderedList listItems ->
                    foldl function (function block acc) remainingBlocks

                OrderedList int lists ->
                    foldl function (function block acc) remainingBlocks

                BlockQuote blocks ->
                    foldl function (function block acc) (blocks ++ remainingBlocks)

                _ ->
                    foldl function (function block acc) remainingBlocks


filter : (Block -> Bool) -> List Block -> List Block
filter isGood list =
    foldl
        (\x xs ->
            if isGood x then
                -- TODO need to handle nesting here
                (::) x xs

            else
                xs
        )
        []
        list
