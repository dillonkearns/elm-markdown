port module OutputMarkdownHtml exposing (main)

import Html.String as Html
import Html.String.Attributes as Attr
import Markdown.Inlines exposing (StyledString)
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


renderMarkdown : String -> Html
renderMarkdown markdown =
    markdown
        |> Markdown.render
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
            , bold =
                \content -> Html.strong [] [ Html.text content ]
            , italic =
                \content -> Html.em [] [ Html.text content ]
            , code =
                \content -> Html.code [] [ Html.text content ]
            , link =
                \link content ->
                    Html.a [ Attr.href link.destination ] [ Html.text content ]
                        |> Ok
            , plain =
                Html.text
            , list =
                \items ->
                    Html.ul []
                        (items
                            |> List.map
                                (\itemBlocks ->
                                    Html.li []
                                        [ itemBlocks ]
                                )
                        )
            , htmlDecoder = Markdown.htmlOneOf []
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
