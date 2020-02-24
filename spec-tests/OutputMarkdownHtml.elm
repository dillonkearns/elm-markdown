port module OutputMarkdownHtml exposing (main)

import Html.String as Html
import Html.String.Attributes as Attr
import Markdown.Html
import Markdown.Inlines
import Markdown.Parser as Markdown


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
        |> Result.andThen (\ast -> Markdown.render renderer ast)


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
                        1 ->
                            Html.h1 [] children

                        2 ->
                            Html.h2 [] children

                        3 ->
                            Html.h3 [] children

                        4 ->
                            Html.h4 [] children

                        5 ->
                            Html.h5 [] children

                        6 ->
                            Html.h6 [] children

                        _ ->
                            Html.text "TODO maye use a type here to clean it up... this will never happen"
            , raw = Html.p []
            , blockQuote = Html.blockquote []
            , bold =
                \content -> Html.strong [] [ Html.text content ]
            , italic =
                \content -> Html.em [] [ Html.text content ]
            , code =
                \content -> Html.code [] [ Html.text content ]
            , link =
                \link content ->
                    Html.a [ Attr.href link.destination ] content
                        |> Ok
            , image =
                \image content ->
                    Html.img [ Attr.src image.src ] [ Html.text content ]
                        |> Ok
            , plain =
                Html.text
            , unorderedList =
                \items ->
                    Html.ul []
                        (items
                            |> List.map
                                (\item ->
                                    case item of
                                        Markdown.ListItem task children ->
                                            let
                                                checkbox =
                                                    case task of
                                                        Markdown.NoTask ->
                                                            Html.text ""

                                                        Markdown.IncompleteTask ->
                                                            Html.input
                                                                [ Attr.disabled True
                                                                , Attr.checked False
                                                                , Attr.type_ "checkbox"
                                                                ]
                                                                []

                                                        Markdown.CompletedTask ->
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
                Markdown.Html.oneOf
                    ([ "table"
                     , "tr"
                     , "td"
                     , "pre"
                     , "th"
                     , "div"
                     , "a"
                     ]
                        |> List.map passThroughNode
                    )
            , codeBlock =
                \{ body, language } ->
                    Html.pre []
                        [ Html.code []
                            [ Html.text body
                            ]
                        ]
            , thematicBreak = Html.hr [] []
            }
        |> Result.map (List.map (Html.toString 0))
        |> Result.map (String.join "")


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
