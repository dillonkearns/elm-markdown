module Markdown.Renderer exposing
    ( Renderer, render
    , defaultHtmlRenderer
    , renderWithMeta
    )

{-|

@docs Renderer, render

@docs defaultHtmlRenderer


## Attaching Metadata to Blocks

@docs renderWithMeta

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Markdown.Block as Block exposing (Block, Inline, ListItem, Task)
import Markdown.Html
import Markdown.HtmlRenderer
import Markdown.RawBlock exposing (Attribute, RawBlock(..), UnparsedInlines(..))


{-| A record with functions that define how to render all possible markdown blocks.
These renderers are composed together to give you the final rendered output.

You could render to any type you want. Here are some useful things you might render to:

  - `Html` (using the `defaultHtmlRenderer` provided by this module)
  - Custom `Html`
  - `Element`s from [`mdgriffith/elm-ui`](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/)
  - Types from other custom HTML replacement libraries, like [`rtfeldman/elm-css`](https://package.elm-lang.org/packages/rtfeldman/elm-css/latest/) or [`tesk9/accessible-html`](https://package.elm-lang.org/packages/tesk9/accessible-html/latest/)
  - Raw `String`s with [ANSI color codes](http://www.lihaoyi.com/post/BuildyourownCommandLinewithANSIescapecodes.html) for setting rich colors in terminal (CLI) output
  - Plain text with any formatting stripped away (maybe for a String search feature)

-}
type alias Renderer view =
    { heading : { level : Block.HeadingLevel, rawText : String, children : List view } -> view
    , paragraph : List view -> view
    , blockQuote : List view -> view
    , html : Markdown.Html.Renderer (List view -> view)
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
defaultHtmlRenderer : Renderer (Html msg)
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
                classes =
                    -- Only the first word is used in the class
                    case Maybe.map String.words language of
                        Just (actualLanguage::_) ->
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


{-| Apply a `Renderer` to turn parsed `Markdown.Block`s into your rendered markdown view.
-}
render :
    Renderer view
    -> List Block
    -> Result String (List view)
render renderer ast =
    ast
        |> renderHelper renderer
        |> combineResults


{-| Render Tuples of Blocks with arbitrary metadata. See `examples/src/Slugs.elm` for a full example that shows how to
add metadata to blocks.

    import Markdown.Parser
    import Markdown.Renderer exposing (defaultHtmlRenderer)

    markdownInput
        |> Markdown.Parser.parse
        |> Result.map gatherHeadingOccurrences
        |> Result.mapError deadEndsToString
        |> Result.andThen
            (\ast ->
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
renderWithMeta : (meta -> Renderer view) -> List ( Block, meta ) -> Result String (List view)
renderWithMeta renderWithMetaFn blocksWithMeta =
    blocksWithMeta
        |> List.filterMap (\( block, meta ) -> renderHelperSingle (renderWithMetaFn meta) block)
        |> combineResults


renderHtml :
    String
    -> List Attribute
    -> List Block
    -> Markdown.Html.Renderer (List view -> view)
    -> List (Result String view)
    -> Result String view
renderHtml tagName attributes children (Markdown.HtmlRenderer.HtmlRenderer htmlRenderer) renderedChildren =
    renderedChildren
        |> combineResults
        |> Result.andThen
            (\okChildren ->
                htmlRenderer tagName attributes children
                    |> Result.map
                        (\myRenderer -> myRenderer okChildren)
            )


combineResults : List (Result x a) -> Result x (List a)
combineResults =
    List.foldr (Result.map2 (::)) (Ok [])


renderHelper :
    Renderer view
    -> List Block
    -> List (Result String view)
renderHelper renderer blocks =
    List.filterMap (renderHelperSingle renderer) blocks


renderHelperSingle : Renderer view -> Block -> Maybe (Result String view)
renderHelperSingle renderer =
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
                    Block.HtmlElement tag attributes children ->
                        renderHtmlNode renderer tag attributes children
                            |> Just

                    _ ->
                        Nothing

            Block.UnorderedList items ->
                items
                    |> List.map
                        (\(Block.ListItem task children) ->
                            children
                                |> renderStyled renderer
                                |> Result.map (\renderedBody -> Block.ListItem task renderedBody)
                        )
                    |> combineResults
                    |> Result.map renderer.unorderedList
                    |> Just

            Block.OrderedList startingIndex items ->
                items
                    |> List.map (renderStyled renderer)
                    |> combineResults
                    |> Result.map (renderer.orderedList startingIndex)
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
                    renderedHeaderCells : Result String (List ( Maybe Block.Alignment, List view ))
                    renderedHeaderCells =
                        header
                            |> List.map
                                (\{ label, alignment } ->
                                    Result.map (Tuple.pair alignment) (renderStyled renderer label)
                                )
                            |> combineResults

                    renderedHeader : Result String view
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

                    renderRow : List (List Inline) -> Result String view
                    renderRow cells =
                        cells
                            |> List.map (renderStyled renderer)
                            |> combineResults
                            |> Result.map (List.indexedMap (\index cell -> renderer.tableCell (alignmentForColumn index) cell))
                            |> Result.map renderer.tableRow

                    renderedRows : Result String (List view)
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


renderStyled : Renderer view -> List Inline -> Result String (List view)
renderStyled renderer styledStrings =
    styledStrings
        |> List.foldr (foldThing renderer) []
        |> combineResults


foldThing : Renderer view -> Inline -> List (Result String view) -> List (Result String view)
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


renderSingleInline : Renderer view -> Block.Inline -> Maybe (Result String view)
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
                Block.HtmlElement tag attributes children ->
                    renderHtmlNode renderer tag attributes children
                        |> Just

                _ ->
                    Nothing


renderHtmlNode : Renderer view -> String -> List Attribute -> List Block -> Result String view
renderHtmlNode renderer tag attributes children =
    renderHtml tag
        attributes
        children
        renderer.html
        (renderHelper renderer children)
