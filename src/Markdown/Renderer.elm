module Markdown.Renderer exposing
    ( Renderer, render, tryRender
    , defaultHtmlRenderer, defaultStringRenderer
    , renderWithMeta
    )

{-|

@docs Renderer, render, tryRender

@docs defaultHtmlRenderer, defaultStringRenderer


## Attaching Metadata to Blocks

@docs renderWithMeta

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Markdown.Block as Block exposing (Block, Inline, ListItem)
import Markdown.Html
import Markdown.HtmlRenderer
import Markdown.RawBlock exposing (Attribute)


{-| A record with functions that define how to render all possible markdown blocks.
These renderers are composed together to give you the final rendered output.

The `err` type parameter tracks whether the HTML renderer can fail. Use `Renderer String view`
with [`tryRender`](#tryRender) when your HTML renderer may produce errors (e.g. unregistered tags).
Use `Renderer Never view` with [`render`](#render) for an infallible pipeline — you can convert
a fallible renderer into an infallible one using [`Markdown.Html.withFallback`](Markdown-Html#withFallback).

You could render to any type you want. Here are some useful things you might render to:

  - `Html` (using the `defaultHtmlRenderer` provided by this module)
  - Custom `Html`
  - `Element`s from [`mdgriffith/elm-ui`](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/)
  - Types from other custom HTML replacement libraries, like [`rtfeldman/elm-css`](https://package.elm-lang.org/packages/rtfeldman/elm-css/latest/) or [`tesk9/accessible-html`](https://package.elm-lang.org/packages/tesk9/accessible-html/latest/)
  - Raw `String`s with [ANSI color codes](http://www.lihaoyi.com/post/BuildyourownCommandLinewithANSIescapecodes.html) for setting rich colors in terminal (CLI) output
  - Plain text with any formatting stripped away (maybe for a String search feature)

-}
type alias Renderer err view =
    { heading : { level : Block.HeadingLevel, rawText : String, children : List view } -> view
    , paragraph : List view -> view
    , blockQuote : List view -> view
    , html : Markdown.Html.Renderer err (List view -> view)
    , text : String -> view
    , codeSpan : String -> view
    , strong : List view -> view
    , emphasis : List view -> view
    , strikethrough : List view -> view
    , hardLineBreak : view
    , link : { title : Maybe String, destination : String } -> List view -> view
    , image : { alt : String, src : String, title : Maybe String } -> view
    , unorderedList : List (ListItem view) -> view
    , orderedList : Int -> List (List view) -> view
    , codeBlock : { body : String, language : Maybe String } -> view
    , thematicBreak : view
    , table : List view -> view
    , tableHeader : List view -> view
    , tableBody : List view -> view
    , tableRow : List view -> view
    , tableCell : Maybe Block.Alignment -> List view -> view
    , tableHeaderCell : Maybe Block.Alignment -> List view -> view
    }


{-| This renders `Html` in an attempt to be as close as possible to
the HTML output in <https://github.github.com/gfm/>.
-}
defaultHtmlRenderer : Renderer String (Html msg)
defaultHtmlRenderer =
    { heading =
        \{ level, children } ->
            case level of
                Block.H1 ->
                    Html.h1 [] children

                Block.H2 ->
                    Html.h2 [] children

                Block.H3 ->
                    Html.h3 [] children

                Block.H4 ->
                    Html.h4 [] children

                Block.H5 ->
                    Html.h5 [] children

                Block.H6 ->
                    Html.h6 [] children
    , paragraph = Html.p []
    , hardLineBreak = Html.br [] []
    , blockQuote = Html.blockquote []
    , strong =
        \children -> Html.strong [] children
    , emphasis =
        \children -> Html.em [] children
    , strikethrough =
        \children -> Html.del [] children
    , codeSpan =
        \content -> Html.code [] [ Html.text content ]
    , link =
        \link content ->
            case link.title of
                Just title ->
                    Html.a
                        [ Attr.href link.destination
                        , Attr.title title
                        ]
                        content

                Nothing ->
                    Html.a [ Attr.href link.destination ] content
    , image =
        \imageInfo ->
            case imageInfo.title of
                Just title ->
                    Html.img
                        [ Attr.src imageInfo.src
                        , Attr.alt imageInfo.alt
                        , Attr.title title
                        ]
                        []

                Nothing ->
                    Html.img
                        [ Attr.src imageInfo.src
                        , Attr.alt imageInfo.alt
                        ]
                        []
    , text =
        Html.text
    , unorderedList =
        \items ->
            Html.ul []
                (items
                    |> List.map
                        (\item ->
                            case item of
                                Block.ListItem task children ->
                                    let
                                        checkbox : Html msg
                                        checkbox =
                                            case task of
                                                Block.NoTask ->
                                                    Html.text ""

                                                Block.IncompleteTask ->
                                                    Html.input
                                                        [ Attr.disabled True
                                                        , Attr.checked False
                                                        , Attr.type_ "checkbox"
                                                        ]
                                                        []

                                                Block.CompletedTask ->
                                                    Html.input
                                                        [ Attr.disabled True
                                                        , Attr.checked True
                                                        , Attr.type_ "checkbox"
                                                        ]
                                                        []
                                    in
                                    Html.li [] (checkbox :: children)
                        )
                )
    , orderedList =
        \startingIndex items ->
            Html.ol
                (case startingIndex of
                    1 ->
                        [ Attr.start startingIndex ]

                    _ ->
                        []
                )
                (items
                    |> List.map
                        (\itemBlocks ->
                            Html.li []
                                itemBlocks
                        )
                )
    , html = Markdown.Html.oneOf []
    , codeBlock =
        \{ body, language } ->
            let
                classes : List (Html.Attribute msg)
                classes =
                    -- Only the first word is used in the class
                    case Maybe.map String.words language of
                        Just (actualLanguage :: _) ->
                            [ Attr.class <| "language-" ++ actualLanguage ]

                        _ ->
                            []
            in
            Html.pre []
                [ Html.code classes
                    [ Html.text body
                    ]
                ]
    , thematicBreak = Html.hr [] []
    , table = Html.table []
    , tableHeader = Html.thead []
    , tableBody = Html.tbody []
    , tableRow = Html.tr []
    , tableHeaderCell =
        \maybeAlignment ->
            let
                attrs : List (Html.Attribute msg)
                attrs =
                    maybeAlignment
                        |> Maybe.map
                            (\alignment ->
                                case alignment of
                                    Block.AlignLeft ->
                                        "left"

                                    Block.AlignCenter ->
                                        "center"

                                    Block.AlignRight ->
                                        "right"
                            )
                        |> Maybe.map Attr.align
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
            in
            Html.th attrs
    , tableCell =
        \maybeAlignment ->
            let
                attrs : List (Html.Attribute msg)
                attrs =
                    maybeAlignment
                        |> Maybe.map
                            (\alignment ->
                                case alignment of
                                    Block.AlignLeft ->
                                        "left"

                                    Block.AlignCenter ->
                                        "center"

                                    Block.AlignRight ->
                                        "right"
                            )
                        |> Maybe.map Attr.align
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
            in
            Html.td attrs
    }


{-| This renders the parsed markdown structs to a string.
-}
defaultStringRenderer : Renderer String String
defaultStringRenderer =
    { heading =
        \{ level, children } ->
            (case level of
                Block.H1 ->
                    "# " ++ String.concat children

                Block.H2 ->
                    "## " ++ String.concat children

                Block.H3 ->
                    "### " ++ String.concat children

                Block.H4 ->
                    "#### " ++ String.concat children

                Block.H5 ->
                    "##### " ++ String.concat children

                Block.H6 ->
                    "###### "
                        ++ String.concat children
            )
                ++ "\n\n"
    , paragraph =
        \strs ->
            String.concat strs
                ++ "\n\n"
    , hardLineBreak = "  \n"
    , blockQuote =
        \strs ->
            strs
                |> List.map (\s -> "  " ++ s ++ "\n")
                |> String.concat
    , strong =
        \s ->
            String.concat
                ("**" :: s ++ [ "**" ])
    , emphasis =
        \s ->
            String.concat
                ("*" :: s ++ [ "*" ])
    , strikethrough =
        \s ->
            String.concat
                ("~~" :: s ++ [ "~~" ])
    , codeSpan =
        \s ->
            "`" ++ s ++ "`"
    , link =
        \link content ->
            String.concat
                [ "["
                , String.concat content
                , "]("
                , link.destination
                , ")"
                ]
    , image =
        \imageInfo ->
            String.concat
                [ "!["
                , imageInfo.alt
                , "]("
                , imageInfo.src
                , ")"
                ]
    , text = identity
    , unorderedList =
        \items ->
            items
                |> List.map
                    (\listitem ->
                        case listitem of
                            Block.ListItem Block.NoTask childs ->
                                "- " ++ String.concat childs ++ "\n"

                            Block.ListItem Block.IncompleteTask childs ->
                                "- [ ]" ++ String.concat childs ++ "\n"

                            Block.ListItem Block.CompletedTask childs ->
                                "- [x]" ++ String.concat childs ++ "\n"
                    )
                |> String.concat
    , orderedList =
        \startingIndex items ->
            items
                |> List.indexedMap (\i item -> String.fromInt (i + startingIndex) ++ ") " ++ String.concat item ++ "\n")
                |> String.concat
    , html = Markdown.Html.oneOf []
    , codeBlock =
        \{ body, language } ->
            String.concat
                [ "```"
                , language |> Maybe.withDefault ""
                , "\n"
                , body
                , "```\n\n"
                ]
    , thematicBreak = "--------------------\n"
    , table = String.concat >> (++) "\n"
    , tableHeader =
        -- we get the whole header as one string here, contained in a single element list.
        List.map
            twoheads
            >> String.concat
    , tableBody = List.foldr (\s l -> s :: "\n" :: l) [] >> String.concat
    , tableRow = List.intersperse " | " >> String.concat
    , tableHeaderCell =
        \maybeAlignment strs ->
            String.concat strs
                ++ " | "
                ++ (case maybeAlignment of
                        Just Block.AlignLeft ->
                            ":-"

                        Just Block.AlignRight ->
                            "-:"

                        Just Block.AlignCenter ->
                            ":-:"

                        Nothing ->
                            "--"
                   )
    , tableCell =
        \_ strs ->
            String.concat strs
    }


twoheads : String -> String
twoheads headstr =
    headstr
        |> String.split " | "
        |> toheads ( [], [] )
        |> (\( heads, aligns ) ->
                String.concat (List.intersperse " | " heads)
                    ++ "\n"
                    ++ String.concat (List.intersperse "|" aligns)
                    ++ "\n"
           )


toheads : ( List String, List String ) -> List String -> ( List String, List String )
toheads ( llst, rlst ) strs =
    case strs of
        l :: r :: cdr ->
            toheads ( l :: llst, r :: rlst ) cdr

        _ ->
            ( List.reverse llst, List.reverse rlst )


{-| Apply an infallible `Renderer` (one whose HTML renderer uses `Never` as its
error type, e.g. via `Markdown.Html.withFallback`)
to turn parsed `Markdown.Block`s into your rendered markdown view.

Since the renderer can never fail, this returns `List view` directly instead of
`Result`.

-}
render :
    Renderer Never view
    -> List Block
    -> List view
render renderer ast =
    case tryRender renderer ast of
        Ok views ->
            views

        Err n ->
            never n


{-| Apply a `Renderer` to turn parsed `Markdown.Block`s into your rendered markdown view.
-}
tryRender :
    Renderer err view
    -> List Block
    -> Result err (List view)
tryRender renderer ast =
    ast
        |> renderHelper renderer
        |> combineResults


{-| Render Tuples of Blocks with arbitrary metadata. See `examples/src/Slugs.elm` for a full example that shows how to
add metadata to blocks.

    import Markdown.Parser
    import Markdown.Renderer exposing (defaultHtmlRenderer)

    markdownInput
        |> Markdown.Parser.parse
        |> gatherHeadingOccurrences
        |> (\ast ->
                Markdown.Renderer.renderWithMeta
                    (\maybeSlug ->
                        { defaultHtmlRenderer
                            | heading =
                                \{ level, children } ->
                                    Html.h1
                                        [ Attr.id (maybeSlug |> Maybe.withDefault "")
                                        ]
                                        children
                        }
                    )
                    ast
           )

-}
renderWithMeta : (meta -> Renderer err view) -> List ( Block, meta ) -> Result err (List view)
renderWithMeta renderWithMetaFn blocksWithMeta =
    blocksWithMeta
        |> List.filterMap (\( block, meta ) -> renderHelperSingle (renderWithMetaFn meta) block)
        |> combineResults


renderHtml :
    String
    -> List Attribute
    -> String
    -> Markdown.Html.Renderer err (List view -> view)
    -> List (Result err view)
    -> Result err view
renderHtml tagName attributes rawBody (Markdown.HtmlRenderer.HtmlRenderer htmlRenderer) renderedChildren =
    renderedChildren
        |> combineResults
        |> Result.andThen
            (\okChildren ->
                htmlRenderer tagName attributes rawBody
                    |> Result.map
                        (\myRenderer -> myRenderer okChildren)
            )


combineResults : List (Result err a) -> Result err (List a)
combineResults =
    List.foldr (Result.map2 (::)) (Ok [])


renderHelper :
    Renderer err view
    -> List Block
    -> List (Result err view)
renderHelper renderer blocks =
    List.filterMap (renderHelperSingle renderer) blocks


renderHelperSingle : Renderer err view -> Block -> Maybe (Result err view)
renderHelperSingle renderer =
    -- known-unoptimized-recursion
    \block ->
        case block of
            Block.Heading level content ->
                renderStyled renderer content
                    |> Result.map
                        (\children ->
                            renderer.heading
                                { level = level
                                , rawText = Block.extractInlineText content
                                , children = children
                                }
                        )
                    |> Just

            Block.Paragraph content ->
                renderStyled renderer content
                    |> Result.map renderer.paragraph
                    |> Just

            Block.HtmlBlock html ->
                case html of
                    Block.HtmlElement tag attributes children raw ->
                        renderHtmlNode renderer tag attributes children raw
                            |> Just

                    Block.ClosingTag tagName ->
                        -- Render closing tag with "/" prefix so user's renderer can handle it
                        renderHtmlNodeEmpty renderer ("/" ++ tagName)
                            |> Just

                    _ ->
                        Nothing

            Block.UnorderedList tight items ->
                items
                    |> List.map
                        (\(Block.ListItem task children) ->
                            children
                                --|> renderHelper renderer
                                |> (\blocks ->
                                        List.filterMap
                                            (\listItemBlock ->
                                                case ( tight, listItemBlock ) of
                                                    ( Block.Tight, Block.Paragraph content ) ->
                                                        renderStyled renderer content |> Just

                                                    _ ->
                                                        renderHelperSingle renderer listItemBlock
                                                            |> Maybe.map (Result.map List.singleton)
                                            )
                                            blocks
                                   )
                                |> combineResults
                                |> Result.map (Block.ListItem task)
                        )
                    |> combineResults
                    |> Result.map
                        (\listItems ->
                            listItems
                                |> List.map
                                    (\(Block.ListItem task children) ->
                                        Block.ListItem task (List.concat children)
                                    )
                                |> renderer.unorderedList
                        )
                    |> Just

            Block.OrderedList tight startingIndex items ->
                items
                    |> List.map
                        (\itemsblocks ->
                            itemsblocks
                                |> (\blocks ->
                                        List.filterMap
                                            (\listItemBlock ->
                                                case ( tight, listItemBlock ) of
                                                    ( Block.Tight, Block.Paragraph content ) ->
                                                        renderStyled renderer content |> Just

                                                    _ ->
                                                        renderHelperSingle renderer listItemBlock
                                                            |> Maybe.map (Result.map List.singleton)
                                            )
                                            blocks
                                   )
                                |> combineResults
                        )
                    |> combineResults
                    |> Result.map
                        (\listItems ->
                            listItems
                                |> List.map
                                    (\children ->
                                        List.concat children
                                    )
                                |> renderer.orderedList startingIndex
                        )
                    |> Just

            Block.CodeBlock codeBlock ->
                codeBlock
                    |> renderer.codeBlock
                    |> Ok
                    |> Just

            Block.ThematicBreak ->
                Ok renderer.thematicBreak
                    |> Just

            Block.BlockQuote nestedBlocks ->
                renderHelper renderer nestedBlocks
                    |> combineResults
                    |> Result.map renderer.blockQuote
                    |> Just

            Block.Table header rows ->
                let
                    renderedHeaderCells : Result err (List ( Maybe Block.Alignment, List view ))
                    renderedHeaderCells =
                        header
                            |> List.map
                                (\{ label, alignment } ->
                                    Result.map (Tuple.pair alignment) (renderStyled renderer label)
                                )
                            |> combineResults

                    renderedHeader : Result err view
                    renderedHeader =
                        renderedHeaderCells
                            |> Result.map
                                (\listListView ->
                                    listListView
                                        |> List.map (\( maybeAlignment, item ) -> renderer.tableHeaderCell maybeAlignment item)
                                        |> renderer.tableRow
                                        |> List.singleton
                                        |> renderer.tableHeader
                                )

                    alignmentForColumn : Int -> Maybe Block.Alignment
                    alignmentForColumn columnIndex =
                        header
                            |> List.drop columnIndex
                            |> List.head
                            |> Maybe.andThen .alignment

                    renderRow : List (List Inline) -> Result err view
                    renderRow cells =
                        cells
                            |> List.map (renderStyled renderer)
                            |> combineResults
                            |> Result.map (List.indexedMap (\index cell -> renderer.tableCell (alignmentForColumn index) cell))
                            |> Result.map renderer.tableRow

                    renderedRows : Result err (List view)
                    renderedRows =
                        rows
                            |> List.map renderRow
                            |> combineResults

                    renderedBody : List view -> List view
                    renderedBody r =
                        if List.isEmpty r then
                            []

                        else
                            [ renderer.tableBody r ]
                in
                Result.map2 (\h r -> renderer.table (h :: renderedBody r)) renderedHeader renderedRows
                    |> Just


renderStyled : Renderer err view -> List Inline -> Result err (List view)
renderStyled renderer styledStrings =
    styledStrings
        |> List.foldr (foldThing renderer) []
        |> combineResults


foldThing : Renderer err view -> Inline -> List (Result err view) -> List (Result err view)
foldThing renderer topLevelInline soFar =
    --                    Ok styledLine ->
    --                        (renderStyled renderer styledLine
    --                            |> Result.andThen
    --                                (\children ->
    --                                    renderer.link { title = link.title, destination = destination } children
    --                                )
    --                        )
    --                            :: soFar
    --
    --                    Err error ->
    --                        (error |> List.map deadEndToString |> List.map Err)
    --                            ++ soFar
    --Block.InlineContent inline ->
    case renderSingleInline renderer topLevelInline of
        Just inline ->
            inline :: soFar

        Nothing ->
            soFar


renderSingleInline : Renderer err view -> Block.Inline -> Maybe (Result err view)
renderSingleInline renderer inline =
    case inline of
        Block.Strong innerInlines ->
            innerInlines
                |> renderStyled renderer
                |> Result.map renderer.strong
                |> Just

        Block.Emphasis innerInlines ->
            innerInlines
                |> renderStyled renderer
                |> Result.map renderer.emphasis
                |> Just

        Block.Strikethrough innerInlines ->
            innerInlines
                |> renderStyled renderer
                |> Result.map renderer.strikethrough
                |> Just

        Block.Image src title children ->
            renderer.image { alt = Block.extractInlineText children, src = src, title = title }
                |> Ok
                |> Just

        Block.Text string ->
            renderer.text string
                |> Ok
                |> Just

        Block.CodeSpan string ->
            renderer.codeSpan string
                |> Ok
                |> Just

        Block.Link destination title inlines ->
            renderStyled renderer inlines
                |> Result.andThen
                    (\children ->
                        renderer.link { title = title, destination = destination } children
                            |> Ok
                    )
                |> Just

        Block.HardLineBreak ->
            renderer.hardLineBreak
                |> Ok
                |> Just

        Block.HtmlInline html ->
            case html of
                Block.HtmlElement tag attributes children raw ->
                    renderInlineHtmlNode renderer tag attributes children raw
                        |> Just

                Block.ClosingTag tagName ->
                    -- Render closing tag with "/" prefix so user's renderer can handle it
                    renderHtmlNodeEmpty renderer ("/" ++ tagName)
                        |> Just

                _ ->
                    Nothing


renderHtmlNode : Renderer err view -> String -> List Attribute -> List Block -> String -> Result err view
renderHtmlNode renderer tag attributes children raw =
    renderHtml tag
        attributes
        raw
        renderer.html
        (renderHelper renderer children)


renderInlineHtmlNode : Renderer err view -> String -> List Attribute -> List Inline -> String -> Result err view
renderInlineHtmlNode renderer tag attributes children raw =
    let
        (Markdown.HtmlRenderer.HtmlRenderer htmlRenderer) =
            renderer.html
    in
    renderStyled renderer children
        |> Result.andThen
            (\renderedChildren ->
                htmlRenderer tag attributes raw
                    |> Result.map (\myRenderer -> myRenderer renderedChildren)
            )


renderHtmlNodeEmpty : Renderer err view -> String -> Result err view
renderHtmlNodeEmpty renderer tag =
    renderHtml tag
        []
        ""
        renderer.html
        []
