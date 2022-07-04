port module OutputMarkdownHtml exposing (main)

import Html.String as Html
import Html.String.Attributes as Attr
import Markdown.Block as Block exposing (Block)
import Markdown.Html
import Markdown.HtmlRenderer
import Markdown.Parser as Markdown
import Markdown.Renderer
import Regex


port requestHtml : (String -> msg) -> Sub msg


port printOutput : String -> Cmd msg


port error : String -> Cmd msg


printHtml : Html -> Cmd msg
printHtml renderResult =
    case renderResult of
        Ok htmlString ->
            printOutput htmlString

        Err errorString ->
            error errorString


type alias Html =
    Result String String


init flags =
    ( ()
    , Cmd.none
    )


render renderer markdown =
    markdown
        |> Markdown.parse
        |> Result.mapError deadEndsToString
        |> Result.andThen (\ast -> Markdown.Renderer.render renderer ast)


deadEndsToString deadEnds =
    deadEnds
        |> List.map Markdown.deadEndToString
        |> String.join "\n"


renderMarkdown : String -> Html
renderMarkdown markdown =
    markdown
        |> render
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
                        (if startingIndex /= 1 then
                            [ Attr.start startingIndex ]

                         else
                            []
                        )
                        (items
                            |> List.map
                                (\itemBlocks ->
                                    Html.li []
                                        itemBlocks
                                )
                        )
            , html =
                htmlRenderer
            , codeBlock =
                \{ body, language } ->
                    let
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
        |> Result.map (List.map (Html.toString 0))
        |> Result.map (String.join "")
        |> Result.map removeVoidClosingTags


{-| Ensure that void tags don't have closing tag, see <https://github.com/zwilias/elm-html-string/issues/12>.
-}
removeVoidClosingTags : String -> String
removeVoidClosingTags string =
    string
        |> Regex.replace voidClosingReplaceRegex (\_ -> "")


voidClosingReplaceRegex =
    Regex.fromString
        ("("
            ++ (voidTags
                    |> List.map (\tagName -> "</" ++ tagName ++ ">")
                    |> String.join "|"
               )
            ++ ")"
        )
        |> Maybe.withDefault Regex.never


{-| <https://html.spec.whatwg.org/multipage/syntax.html#void-elements>
-}
voidTags =
    [ "area"
    , "base"
    , "br"
    , "col"
    , "embed"
    , "hr"
    , "img"
    , "input"
    , "link"
    , "meta"
    , "param"
    , "source"
    , "track"
    , "wbr"
    ]


htmlRenderer : Markdown.Html.Renderer (List (Html.Html msg) -> Html.Html msg)
htmlRenderer =
    passthrough
        (\tag attributes blocks ->
            let
                result : Result String (List (Html.Html msg) -> Html.Html msg)
                result =
                    (\children ->
                        Html.node tag htmlAttributes children
                    )
                        |> Ok

                htmlAttributes : List (Html.Attribute msg)
                htmlAttributes =
                    attributes
                        |> List.map
                            (\{ name, value } ->
                                Attr.attribute name value
                            )
            in
            result
        )


passThroughNode nodeName =
    Markdown.Html.tag nodeName
        (\id class href children ->
            Html.node nodeName
                ([ id |> Maybe.map Attr.id
                 , class |> Maybe.map Attr.class
                 , href |> Maybe.map Attr.href
                 ]
                    |> List.filterMap identity
                )
                children
        )
        |> Markdown.Html.withOptionalAttribute "id"
        |> Markdown.Html.withOptionalAttribute "class"
        |> Markdown.Html.withOptionalAttribute "href"


{-| TODO come up with an API to provide a solution to do this sort of thing publicly
-}
passthrough : (String -> List Markdown.HtmlRenderer.Attribute -> List Block -> Result String view) -> Markdown.HtmlRenderer.HtmlRenderer view
passthrough renderFn =
    Markdown.HtmlRenderer.HtmlRenderer renderFn


type Msg
    = RequestedHtml String


type alias Model =
    ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestedHtml markdown ->
            ( model
            , markdown
                |> renderMarkdown
                |> printHtml
            )


main : Program () () Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = \model -> requestHtml RequestedHtml
        }
