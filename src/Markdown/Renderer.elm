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
    , hardLineBreak : view
    , link : { title : Maybe String, destination : String } -> List view -> view
    , image : { alt : String, src : String, title : Maybe String } -> view
    , unorderedList : List (ListItem view) -> view
    , orderedList : Int -> List (List view) -> view
    , codeBlock : { body : String, language : Maybe String } -> view
    , thematicBreak : view
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
            Html.pre []
                [ Html.code []
                    [ Html.text body
                    ]
                ]
    , thematicBreak = Html.hr [] []
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
